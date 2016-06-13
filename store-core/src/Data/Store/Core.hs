{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Store.Core
    ( -- * Core Types
      Poke(..), PokeException(..), pokeException
    , Peek(..), PeekException(..), peekException, tooManyBytes
    , Offset
      -- * Encode ByteString
    , unsafeEncodeWith
      -- * Decode ByteString
    , decodeWith
    , decodeExWith, decodeExPortionWith
    , decodeIOWith, decodeIOPortionWith
    , decodeIOWithFromPtr, decodeIOPortionWithFromPtr
      -- * Storable
    , pokeStorable, peekStorable, peekStorableTy
      -- * ForeignPtr
    , pokeFromForeignPtr, peekToPlainForeignPtr, pokeFromPtr
      -- * ByteArray
    , pokeFromByteArray, peekToByteArray
    ) where

import           Control.Applicative
import           Control.Exception (Exception(..), throwIO, try)
import           Control.Monad (when)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.Monoid ((<>))
import           Data.Primitive.ByteArray
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Ptr (plusPtr, minusPtr, castPtr)
import           Foreign.Storable as Storable
import           GHC.Prim (unsafeCoerce#, RealWorld, copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.Types (IO(..), Int(..))
import           Prelude
import           System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- Helpful Type Synonyms

-- | How far into the given Ptr to look
type Offset = Int

------------------------------------------------------------------------
-- Poke monad

-- | 'Poke' actions are useful for building sequential serializers.
--
-- They are actions which write values to bytes into memory specified by
-- a 'Ptr' base. The 'Applicative' and 'Monad' instances make it easy to
-- write serializations, by keeping track of the 'Offset' of the current
-- byte. They allow you to chain 'Poke' action such that subsequent
-- 'Poke's write into subsequent portions of the output.
newtype Poke a = Poke
    { runPoke :: forall byte. Ptr byte -> Offset -> IO (Offset, a)
      -- ^ Run the 'Poke' action, with the 'Ptr' to the buffer where
      -- data is poked, and the current 'Offset'. The result is the new
      -- offset, along with a return value.
      --
      -- May throw a 'PokeException', though this should be avoided when
      -- possible.  They usually indicate a programming error.
    }
    deriving Functor

instance Applicative Poke where
    pure x = Poke $ \_ptr offset -> pure (offset, x)
    {-# INLINE pure #-}
    Poke f <*> Poke g = Poke $ \ptr offset1 -> do
        (offset2, f') <- f ptr offset1
        (offset3, g') <- g ptr offset2
        return (offset3, f' g')
    {-# INLINE (<*>) #-}
    Poke f *> Poke g = Poke $ \ptr offset1 -> do
        (offset2, _) <- f ptr offset1
        g ptr offset2
    {-# INLINE (*>) #-}

instance Monad Poke where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Poke x >>= f = Poke $ \ptr offset1 -> do
        (offset2, x') <- x ptr offset1
        runPoke (f x') ptr offset2
    {-# INLINE (>>=) #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Poke where
    fail = pokeException . T.pack
    {-# INLINE fail #-}

instance MonadIO Poke where
    liftIO f = Poke $ \_ offset -> (offset, ) <$> f
    {-# INLINE liftIO #-}

-- | Exception thrown while running 'poke'. Note that other types of
-- exceptions could also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PokeException's are not expected to occur in ordinary circumstances,
-- and usually indicate a programming error.
data PokeException = PokeException
    { pokeExByteIndex :: Offset
    , pokeExMessage :: T.Text
    }
    deriving (Eq, Show, Typeable)

instance Exception PokeException where
#if MIN_VERSION_base(4,8,0)
    displayException (PokeException offset msg) =
        "Exception while poking, at byte index " ++
        show offset ++
        " : " ++
        T.unpack msg
#endif

-- | Throws a 'PokeException'. These should be avoided when possible,
-- they usually indicate a programming error.
pokeException :: T.Text -> Poke a
pokeException msg = Poke $ \_ off -> throwIO (PokeException off msg)

------------------------------------------------------------------------
-- Peek monad

-- | 'Peek' actions are useful for building sequential deserializers.
--
-- They are actions which read from memory and construct values from it.
-- The 'Applicative' and 'Monad' instances make it easy to chain these
-- together to get more complicated deserializers. This machinery keeps
-- track of the current 'Ptr' and end-of-buffer 'Ptr'.
newtype Peek a = Peek
    { runPeek :: forall byte. Ptr byte -> Ptr byte -> IO (Ptr byte, a)
      -- ^ Run the 'Peek' action, with a 'Ptr' to the end of the buffer
      -- where data is poked, and a 'Ptr' to the current position. The
      -- result is the 'Ptr', along with a return value.
      --
      -- May throw a 'PeekException' if the memory contains invalid
      -- values.
    }
   deriving Functor

instance Applicative Peek where
    pure x = Peek (\_ ptr -> return (ptr, x))
    {-# INLINE pure #-}
    Peek f <*> Peek g = Peek $ \end ptr1 -> do
        (ptr2, f') <- f end ptr1
        (ptr3, g') <- g end ptr2
        return (ptr3, f' g')
    {-# INLINE (<*>) #-}
    Peek f *> Peek g = Peek $ \end ptr1 -> do
        (ptr2, _) <- f end ptr1
        g end ptr2
    {-# INLINE (*>) #-}

instance Monad Peek where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Peek x >>= f = Peek $ \end ptr1 -> do
        (ptr2, x') <- x end ptr1
        runPeek (f x') end ptr2
    {-# INLINE (>>=) #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Peek where
    fail = peekException . T.pack
    {-# INLINE fail #-}

instance PrimMonad Peek where
    type PrimState Peek = RealWorld
    primitive action = Peek $ \_ ptr -> do
        x <- primitive (unsafeCoerce# action)
        return (ptr, x)
    {-# INLINE primitive #-}

instance MonadIO Peek where
    liftIO f = Peek $ \_ ptr -> (ptr, ) <$> f
    {-# INLINE liftIO #-}

-- | Exception thrown while running 'peek'. Note that other types of
-- exceptions can also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PeekException' is thrown when the data being decoded is invalid.
data PeekException = PeekException
    { peekExBytesFromEnd :: Offset
    , peekExMessage :: T.Text
    } deriving (Eq, Show, Typeable)

instance Exception PeekException where
#if MIN_VERSION_base(4,8,0)
    displayException (PeekException offset msg) =
        "Exception while peeking, " ++
        show offset ++
        " bytes from end: " ++
        T.unpack msg
#endif

-- | Throws a 'PeekException'.
peekException :: T.Text -> Peek a
peekException msg = Peek $ \end ptr -> throwIO (PeekException (end `minusPtr` ptr) msg)

-- | Throws a 'PeekException' about an attempt to read too many bytes.
tooManyBytes :: Int -> Int -> String -> IO void
tooManyBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read too many bytes for " ++
        ty ++
        ". Needed " ++
        show needed ++ ", but only " ++
        show remaining ++ " remain."

------------------------------------------------------------------------
-- Decoding and encoding ByteStrings


-- | Given a 'Poke' and its length, uses it to fill a 'ByteString'
--
-- This function is unsafe because the provided length must exactly
-- match the number of bytes used by the 'Poke'. It will throw
-- 'PokeException' errors when the buffer is under or overshot. However,
-- in the case of overshooting the buffer, memory corruption and
-- segfaults may occur.
unsafeEncodeWith :: Poke () -> Int -> ByteString
unsafeEncodeWith f l = BS.unsafeCreate l $ \p -> do
    (o, ()) <- runPoke f p 0
    checkOffset o l
{-# INLINE unsafeEncodeWith #-}

-- | Checks if the offset matches the expected length, and throw a
-- 'PokeException' otherwise.
checkOffset :: Int -> Int -> IO ()
checkOffset o l
    | o > l = throwIO $ PokeException o $ T.pack $
        "encode overshot end of " ++
        show l ++
        " byte long buffer"
    | o < l = throwIO $ PokeException o $ T.pack $
        "encode undershot end of " <>
        show l <>
        " byte long buffer"
    | otherwise = return ()

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeWith :: Peek a -> ByteString -> Either PeekException a
decodeWith mypeek = unsafePerformIO . try . decodeIOWith mypeek
{-# INLINE decodeWith #-}

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeExWith :: Peek a -> ByteString -> a
decodeExWith f = unsafePerformIO . decodeIOWith f
{-# INLINE decodeExWith #-}

-- | Similar to 'decodeExWith', but it allows there to be more of the
-- buffer remaining. The 'Offset' of the buffer contents immediately
-- after the decoded value is returned.
decodeExPortionWith :: Peek a -> ByteString -> (Offset, a)
decodeExPortionWith f = unsafePerformIO . decodeIOPortionWith f
{-# INLINE decodeExPortionWith #-}

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeIOWith :: Peek a -> ByteString -> IO a
decodeIOWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOWithFromPtr mypeek ptr len
{-# INLINE decodeIOWith #-}

-- | Similar to 'decodeExPortionWith', but runs in the 'IO' monad.
decodeIOPortionWith :: Peek a -> ByteString -> IO (Offset, a)
decodeIOPortionWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOPortionWithFromPtr mypeek ptr len
{-# INLINE decodeIOPortionWith #-}

-- | Like 'decodeIOWith', but using 'Ptr' and length instead of a
-- 'ByteString'.
decodeIOWithFromPtr :: Peek a -> Ptr Word8 -> Int -> IO a
decodeIOWithFromPtr mypeek ptr len = do
    (offset, x) <- decodeIOPortionWithFromPtr mypeek ptr len
    if len /= offset
       then throwIO $ PeekException (len - offset) "Didn't consume all input."
       else return x
{-# INLINE decodeIOWithFromPtr #-}

-- | Like 'decodeIOPortionWith', but using 'Ptr' and length instead of a 'ByteString'.
decodeIOPortionWithFromPtr :: Peek a -> Ptr Word8 -> Int -> IO (Offset, a)
decodeIOPortionWithFromPtr mypeek ptr len =
    let end = ptr `plusPtr` len
    in do
        (ptr2, x') <- runPeek mypeek end ptr
        if ptr2 > end
            then throwIO $ PeekException (end `minusPtr` ptr2) "Overshot end of buffer"
            else return (ptr2 `minusPtr` ptr, x')
{-# INLINE decodeIOPortionWithFromPtr #-}

------------------------------------------------------------------------
-- Utilities for defining 'Store' instances based on 'Storable'

-- | A 'poke' implementation based on an instance of 'Storable'.
pokeStorable :: Storable a => a -> Poke ()
pokeStorable x = Poke $ \ptr offset -> do
    y <- pokeByteOff ptr offset x
    let !newOffset = offset + sizeOf x
    return (newOffset, y)
{-# INLINE pokeStorable #-}

-- FIXME: make it the responsibility of the caller to check this.

-- | A 'peek' implementation based on an instance of 'Storable' and
-- 'Typeable'.
peekStorable :: forall a. (Storable a, Typeable a) => Peek a
peekStorable = peekStorableTy (show (typeRep (Proxy :: Proxy a)))
{-# INLINE peekStorable #-}

-- | A 'peek' implementation based on an instance of 'Storable'. Use
-- this if the type is not 'Typeable'.
peekStorableTy :: forall a. Storable a => String -> Peek a
peekStorableTy ty = Peek $ \end ptr ->
    let ptr' = ptr `plusPtr` needed
        needed = sizeOf (undefined :: a)
        remaining = end `minusPtr` ptr
     in do
        when (ptr' > end) $
            tooManyBytes needed remaining ty
        x <- Storable.peek (castPtr ptr)
        return (ptr', x)
{-# INLINE peekStorableTy #-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances via memcpy

-- | Copy a section of memory, based on a 'ForeignPtr', to the output.
-- Note that this operation is unsafe, the offset and length parameters
-- are not checked.
pokeFromForeignPtr :: ForeignPtr a -> Int -> Int -> Poke ()
pokeFromForeignPtr sourceFp sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- | Allocate a plain ForeignPtr (no finalizers), of the specified
-- length and fill it with bytes from the input.
peekToPlainForeignPtr :: String -> Int -> Peek (ForeignPtr a)
peekToPlainForeignPtr ty len =
    Peek $ \end sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
        when (ptr2 > end) $
            tooManyBytes len (end `minusPtr` sourcePtr) ty
        fp <- BS.mallocByteString len
        withForeignPtr fp $ \targetPtr ->
            BS.memcpy targetPtr (castPtr sourcePtr) len
        return (ptr2, castForeignPtr fp)

-- | Copy a section of memory, based on a 'Ptr', to the output. Note
-- that this operation is unsafe, because the offset and length
-- parameters are not checked.
pokeFromPtr :: Ptr a -> Int -> Int -> Poke ()
pokeFromPtr sourcePtr sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- TODO: have a safer variant with the check?

-- | Copy a section of memory, based on a 'ByteArray#', to the output.
-- Note that this operation is unsafe, because the offset and length
-- parameters are not checked.
pokeFromByteArray :: ByteArray# -> Int -> Int -> Poke ()
pokeFromByteArray sourceArr sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        let target = targetPtr `plusPtr` targetOffset
        copyByteArrayToAddr sourceArr sourceOffset target len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- | Allocate a ByteArray of the specified length and fill it with bytes
-- from the input.
peekToByteArray :: String -> Int -> Peek ByteArray
peekToByteArray ty len =
    Peek $ \end sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
        when (ptr2 > end) $
            tooManyBytes len (end `minusPtr` sourcePtr) ty
        marr <- newByteArray len
        copyAddrToByteArray sourcePtr marr 0 len
        x <- unsafeFreezeByteArray marr
        return (ptr2, x)

-- | Wrapper around @copyByteArrayToAddr#@ primop.
copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))

-- | Wrapper around @copyAddrToByteArray#@ primop.
copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
