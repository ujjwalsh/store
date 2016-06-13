{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
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
      Poke, PokeException, pokeException
    , Peek, PeekException, peekException
    , Offset
      -- * Encode ByteString
    , unsafeEncodeWith
      -- * Decode ByteString
    , decodeWith, decodeExWith
    , decodeIOWith, decodeIOPortionWith
    , decodeIOWithFromPtr, decodeIOPortionWithFromPtr
      -- * Storable
    , sizeStorable, pokeStorable, peekStorable
      -- * Size
    , Size(..), getSizeWith, contramapSize, combineSizeWith, addSize
      -- * ForeignPtr
    , pokeFromForeignPtr, peekToPlainForeignPtr, pokeFromPtr
      -- * ByteArray
    , pokeFromByteArray, peekToByteArray
    ) where

import           Control.Exception (throwIO, try)
import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.Primitive.ByteArray
import           Data.Store.Core.Internal
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Ptr (plusPtr, minusPtr, castPtr)
import           Foreign.Storable as Storable
import           GHC.Ptr (Ptr(..))
import           System.IO.Unsafe (unsafePerformIO)

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

-- | A 'size' implementation based on an instance of 'Storable' and
-- 'Typeable'.
sizeStorable :: forall a. (Storable a, Typeable a) => Size a
sizeStorable = sizeStorableTy (show (typeRep (Proxy :: Proxy a)))
{-# INLINE sizeStorable #-}

-- | A 'size' implementation based on an instance of 'Storable'. Use this
-- if the type is not 'Typeable'.
sizeStorableTy :: forall a. Storable a => String -> Size a
sizeStorableTy ty = ConstSize (sizeOf (error msg :: a))
  where
    msg = "In Data.Store.storableSize: " ++ ty ++ "'s sizeOf evaluated its argument."
{-# INLINE sizeStorableTy #-}

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
-- Size type

-- | Info about a type's serialized length. Either the length is known
-- independently of the value, or the length depends on the value.
data Size a
    = VarSize (a -> Int)
    | ConstSize !Int
    deriving Typeable

-- | Given a 'Size' value and a value of the type @a@, returns its 'Int'
-- size.
getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n
{-# INLINE getSizeWith #-}

-- | This allows for changing the type used as an input when the 'Size'
-- is 'VarSize'.
contramapSize :: (a -> b) -> Size b -> Size a
contramapSize f (VarSize g) = VarSize (g . f)
contramapSize _ (ConstSize n) = ConstSize n
{-# INLINE contramapSize #-}

-- | Create an aggregate 'Size' by providing functions to split the
-- input into two pieces, as well as 'Size' values to use to measure the
-- results.
--
-- If both of the input 'Size' values are 'ConstSize', the result is
-- 'ConstSize' and the functions will not be used.
combineSizeWith :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
combineSizeWith toA toB sizeA sizeB =
    case (sizeA, sizeB) of
        (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
        (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
        (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE combineSizeWith #-}

-- | Adds a constant amount to a 'Size' value.
addSize :: Int -> Size a -> Size a
addSize x (ConstSize n) = ConstSize (x + n)
addSize x (VarSize f) = VarSize ((x +) . f)
{-# INLINE addSize #-}

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
