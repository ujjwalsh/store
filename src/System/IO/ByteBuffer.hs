{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module: System.IO.ByteBuffer
Description: Provides an efficient buffering abstraction.

A 'ByteBuffer' is a simple buffer for bytes.  It supports two
operations: refilling with the contents of a 'ByteString', and
consuming a fixed number of bytes.

It is implemented as a pointer, together with counters that keep track
of the offset and the number of bytes in the buffer.  Note that the
counters are simple 'IORef's, so 'ByteBuffer's are not thread-safe!

A 'ByteBuffer' is constructed by 'new' with a given starting length,
and will grow (by repeatedly multiplying its size by 1.5) whenever it
is being fed a 'ByteString' that is too large.
-}

module System.IO.ByteBuffer
       ( ByteBuffer
         -- * Allocation and Deallocation
       , new, free
         -- * Query for number of available bytes
       , totalSize, isEmpty, availableBytes
         -- * Feeding new input
       , copyByteString
         -- * Consuming bytes from the buffer
       , consume, unsafeConsume
       ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.IORef
import           Data.Word
import           Foreign.ForeignPtr
import qualified Foreign.Marshal.Alloc as Alloc
import           Foreign.Marshal.Utils hiding (new)
import           GHC.Ptr

-- | A buffer into which bytes can be written.
--
-- Invariants:
--
-- * @size >= containedBytes >= consumedBytes >= 0@
--
-- * The range from @ptr@ to @ptr `plusPtr` size@ will be allocated
--
-- * The range from @ptr@ to @ptr `plusPtr` containedBytes@ will
--   contain bytes previously copied to the buffer
--
-- * The buffer contains @containedBytes - consumedBytes@ bytes of
--   data that have been copied to it, but not yet read.  They are in
--   the range from @ptr `plusPtr` consumedBytes@ to @ptr `plusPtr`
--   containedBytes@.

data BBRef = BBRef {
      size      :: {-# UNPACK #-} !Int
      -- ^ The amount of memory allocated.
    , contained :: {-# UNPACK #-} !Int
      -- ^ The number of bytes that the 'ByteBuffer' currently holds.
    , consumed  :: {-# UNPACK #-} !Int
      -- ^ The number of bytes that have already been consumed.
    , ptr       :: {-# UNPACK #-} !(Ptr Word8)
      -- ^ This points to the beginning of the memory allocated for
      -- the 'ByteBuffer'
    }

type ByteBuffer = IORef BBRef

totalSize :: MonadIO m => ByteBuffer -> m Int
totalSize bb = liftIO $ size <$> readIORef bb

isEmpty :: MonadIO m => ByteBuffer -> m Bool
isEmpty bb = liftIO $ (==0) <$> availableBytes bb

-- | Number of available bytes in a 'ByteBuffer' (that is, bytes that
-- have been copied to, but not yet read from the 'ByteBuffer'.
availableBytes :: MonadIO m => ByteBuffer -> m Int
availableBytes bb = do
    BBRef{..} <- liftIO $ readIORef bb
    return $ contained - consumed

-- | The number of bytes that can be appended to the buffer, without
-- resetting it.
freeCapacity :: MonadIO m => ByteBuffer -> m Int
freeCapacity bb = do
    BBRef{..} <- liftIO $ readIORef bb
    return $ size - contained

-- | Allocates a new ByteBuffer with a given buffer size filling from
-- the given FillBuffer.
new :: MonadIO m
    => Int -- ^ Size of buffer to allocate.
    -> m ByteBuffer -- ^ The byte buffer.
new l = liftIO $ do
    newPtr <- Alloc.mallocBytes l
    newIORef BBRef { ptr = newPtr
                   , size = l
                   , contained = 0
                   , consumed = 0
                   }

-- | Free a byte buffer.
free :: ByteBuffer -> IO ()
free bb = readIORef bb >>= Alloc.free . ptr

-- | Reset a 'ByteBuffer', i.e. copy all the bytes that have not yet
-- been consumed to the front of the buffer.
reset :: ByteBuffer -> IO ()
reset bb = do
    bbref@BBRef{..} <- readIORef bb
    let available = contained - consumed
    moveBytes ptr (ptr `plusPtr` consumed) available
    writeIORef bb bbref { contained = available
                        , consumed = 0
                        }

-- | Make sure the buffer is at least @minSize@ bytes long.
--
-- In order to avoid havong to enlarge the buffer too often, we double
-- its size until it is at least @minSize@ bytes long.
enlargeByteBuffer :: ByteBuffer
                 -> Int
                 -- ^ minSize
                 -> IO ()
enlargeByteBuffer bb minSize = do
    bbref@BBRef{..} <- readIORef bb
    when (size < minSize) $ do
        let newSize = head . dropWhile (<minSize) $
                      iterate (ceiling . (*(1.5 :: Double)) . fromIntegral) (max 1 (size))
        -- possible optimisation: since reallocation might copy the
        -- bytes anyway, we could discard the consumed bytes,
        -- basically 'reset'ting the buffer on the fly.
        -- ptr' <- Alloc.mallocBytes newSize
        ptr' <- Alloc.reallocBytes ptr newSize
        writeIORef bb bbref { ptr = ptr'
                            , size = newSize
                            }

-- | Copy the contents of a 'ByteString' to a 'ByteBuffer'.
--
-- If necessary, the 'ByteBuffer' is enlarged and/or already consumed
-- bytes are dropped.
copyByteString :: MonadIO m => ByteBuffer -> ByteString -> m ()
copyByteString bb bs@(BS.PS _ _ bsSize) = liftIO $ do
    BBRef{..} <- readIORef bb
    -- if the byteBuffer is too small, resize it.
    available <- availableBytes bb -- bytes not yet consumed
    when (size < bsSize + available) (enlargeByteBuffer bb (bsSize + available))
    -- if it is currently too full, reset it
    capacity <- freeCapacity bb
    when (capacity < bsSize) (reset bb)
    -- now we can safely copy.
    unsafeCopyByteString bb bs

-- | Copy the contents of a 'ByteString' to a 'ByteBuffer'. No bounds
-- checks are performed.
unsafeCopyByteString :: ByteBuffer -> ByteString -> IO ()
unsafeCopyByteString bb (BS.PS bsFptr bsOffset bsSize) = do
    bbref@BBRef{..} <- readIORef bb
    withForeignPtr bsFptr $ \ bsPtr ->
        copyBytes (ptr `plusPtr` contained)
                  (bsPtr `plusPtr` bsOffset)
                  bsSize
    writeIORef bb bbref { contained = (contained + bsSize) }

-- | Try to get a pointer to @n@ bytes from the 'ByteBuffer'.
--
-- Note that the pointer should be used before any other actions are
-- performed on the 'ByteBuffer'. It points to some address within the
-- buffer, so operations such as enlarging the buffer or feeding it
-- new data will change the data the pointer points to.  This is why
-- this function is called unsafe.
unsafeConsume :: MonadIO m
        => ByteBuffer
        -> Int
        -- ^ n
        -> m (Either Int (Ptr Word8))
        -- ^ Will be @Left missing@ when there are only @n-missing@
        -- bytes left in the 'ByteBuffer'.
unsafeConsume bb n = liftIO $ do
    available <- availableBytes bb
    if available < n
        then return $ Left (n - available)
        else do
             bbref@BBRef{..} <- readIORef bb
             writeIORef bb bbref { consumed = (consumed + n) }
             return $ Right (ptr `plusPtr` consumed)

-- | As `unsafeConsume`, but instead of returning a `Ptr` into the
-- contents of the `ByteBuffer`, it returns a `ByteString` containing
-- the next @n@ bytes in the buffer.  This involves allocating a new
-- 'ByteString' and copying the @n@ bytes to it.
consume :: MonadIO m
        => ByteBuffer
        -> Int
        -> m (Either Int ByteString)
consume bb n = do
    mPtr <- unsafeConsume bb n
    case mPtr of
        Right ptr -> do
            bs <- liftIO . BS.create n $ \ bsPtr ->
                copyBytes bsPtr ptr n
            return (Right bs)
        Left missing -> return (Left missing)
