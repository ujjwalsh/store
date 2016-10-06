{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
       , new, free, with
         -- * Query for number of available bytes
       , totalSize, isEmpty, availableBytes
         -- * Feeding new input
       , copyByteString
       , fillFromFd
         -- * Consuming bytes from the buffer
       , consume, unsafeConsume
       ) where

import           Control.Applicative
import           Control.Exception.Lifted (bracket, throwIO)
import           Control.Monad (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.IORef
import           Data.Maybe (fromMaybe)
import           Data.Word
import qualified Foreign.C.Error as CE
import           Foreign.ForeignPtr
import qualified Foreign.Marshal.Alloc as Alloc
import           Foreign.Marshal.Utils hiding (new, with)
import           GHC.Ptr
import           Prelude
import Foreign.C.Types
import           System.Posix.Types (Fd (..))

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
{-# INLINE totalSize #-}

isEmpty :: MonadIO m => ByteBuffer -> m Bool
isEmpty bb = liftIO $ (==0) <$> availableBytes bb
{-# INLINE isEmpty #-}

-- | Number of available bytes in a 'ByteBuffer' (that is, bytes that
-- have been copied to, but not yet read from the 'ByteBuffer'.
availableBytes :: MonadIO m => ByteBuffer -> m Int
availableBytes bb = do
    BBRef{..} <- liftIO $ readIORef bb
    return $ contained - consumed
{-# INLINE availableBytes #-}

-- | The number of bytes that can be appended to the buffer, without
-- resetting it.
freeCapacity :: MonadIO m => ByteBuffer -> m Int
freeCapacity bb = do
    BBRef{..} <- liftIO $ readIORef bb
    return $ size - contained
{-# INLINE freeCapacity #-}

-- | Allocates a new ByteBuffer with a given buffer size filling from
-- the given FillBuffer.
--
-- Note that 'ByteBuffer's created with 'new' have to be deallocated
-- explicitly using 'free'.  For automatic deallocation, consider
-- using 'with' instead.
new :: MonadIO m
    => Maybe Int
    -- ^ Size of buffer to allocate.  If 'Nothing', use the default
    -- value of 4MB
    -> m ByteBuffer
    -- ^ The byte buffer.
new ml = liftIO $ do
    let l = fromMaybe (4*1024*1024) ml
    newPtr <- Alloc.mallocBytes l
    newIORef BBRef { ptr = newPtr
                   , size = l
                   , contained = 0
                   , consumed = 0
                   }
{-# INLINE new #-}

-- | Free a byte buffer.
free :: MonadIO m => ByteBuffer -> m ()
free bb = liftIO $ readIORef bb >>= Alloc.free . ptr
{-# INLINE free #-}

-- | Perform some action with a bytebuffer, with automatic allocation
-- and deallocation.
with :: (MonadIO m, MonadBaseControl IO m)
     => Maybe Int
     -- ^ Initial length of the 'ByteBuffer'.  If 'Nothing', use the
     -- default value of 4MB.
     -> (ByteBuffer -> m a)
     -> m a
with l action =
  bracket
    (new l)
    free
    action
{-# INLINE with #-}

-- | Reset a 'ByteBuffer', i.e. copy all the bytes that have not yet
-- been consumed to the front of the buffer.
reset :: ByteBuffer -> IO ()
reset bb = readIORef bb >>= resetBBRef >>= writeIORef bb
{-# INLINE reset #-}

resetBBRef :: BBRef -> IO BBRef
resetBBRef bbref@BBRef{..} = do
    let available = contained - consumed
    moveBytes ptr (ptr `plusPtr` consumed) available
    return bbref { contained = available
                 , consumed = 0
                 }
{-# INLINE resetBBRef #-}

-- | Make sure the buffer is at least @minSize@ bytes long.
--
-- In order to avoid havong to enlarge the buffer too often, we double
-- its size until it is at least @minSize@ bytes long.
enlargeByteBuffer :: ByteBuffer
                 -> Int
                 -- ^ minSize
                 -> IO ()
enlargeByteBuffer bb minSize = readIORef bb >>= (`enlargeBBRef` minSize) >>= writeIORef bb
{-# INLINE enlargeByteBuffer #-}

enlargeBBRef :: BBRef -> Int -> IO BBRef
enlargeBBRef bbref@BBRef{..} minSize =
    if size < minSize
    then do
        let newSize = head . dropWhile (<minSize) $
                      iterate (ceiling . (*(1.5 :: Double)) . fromIntegral) (max 1 size)
        -- possible optimisation: since reallocation might copy the
        -- bytes anyway, we could discard the consumed bytes,
        -- basically 'reset'ting the buffer on the fly.
        -- ptr' <- Alloc.mallocBytes newSize
        ptr' <- Alloc.reallocBytes ptr newSize
        return bbref { ptr = ptr'
                     , size = newSize
                     }
    else return bbref
{-# INLINE enlargeBBRef #-}

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
{-# INLINE copyByteString #-}

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
{-# INLINE unsafeCopyByteString #-}

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
{-# INLINE unsafeConsume #-}

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
{-# INLINE consume #-}

-- | Will read all available bytes from the given socket, enlarging the buffer if necessary.
--
-- Does nothing if the read would block.
fillFromFd :: MonadIO m => ByteBuffer -> Fd -> m Bool
fillFromFd bb sock = liftIO $ do
    bbref <- readIORef bb
    (bbref', bool) <- fillBBRefFromFd sock bbref
    writeIORef bb bbref'
    return bool
{-# INLINE fillFromFd #-}

fillBBRefFromFd :: Fd -> BBRef -> IO (BBRef, Bool)
fillBBRefFromFd (Fd sock) bbref@BBRef{..} = do
    let space = size - contained
    if space == 0
        then do
        bbref' <- if consumed > 0
                 then resetBBRef bbref
                 else enlargeBBRef bbref (size + 1)
        fillBBRefFromFd (Fd sock) bbref'
        else do
          bytes <- fromIntegral <$> c_recv sock (castPtr (ptr `plusPtr` contained)) (fromIntegral space) 0
          if bytes == -1
              then CE.getErrno >>= \case -- encountered an error
                  err | err == CE.eAGAIN || err == CE.eWOULDBLOCK -> return (bbref, False)
                  err -> throwIO $ CE.errnoToIOError "ByteBuffer.fillBBRefFromFd: " err Nothing Nothing
              else let bbref' = bbref{ contained = contained + bytes }
                   in if bytes == space
                      then do
                           (bbref'', _) <- fillBBRefFromFd (Fd sock) bbref'
                           return (bbref'', True)
                      else return (bbref', True)
{-# INLINE fillBBRefFromFd #-}

foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
