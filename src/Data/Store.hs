{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Store
    ( Store(..), Size(..), Poke, Peek
    , encode, decode
      -- * Exception thrown by Peek
    , PeekException(..), peekException
      -- * Utilities for defining 'Store' instances based on 'Storable'
    , sizeStorable
    , peekStorable
    , pokeStorable
      -- * Utilities for defining list-like 'Store' instances
      -- ** In terms of 'Foldable'
    , sizeListLike
    , pokeListLike
    , sizeListLike'
    , pokeListLike'
      -- ** In terms of mutation
    , peekMutableListLike
      -- ** In terms of 'MonoFoldable'
    -- , sizeMonoListLike
    -- , pokeMonoListLike
    ) where

import           Control.Exception (Exception, throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimMonad (..), liftPrim)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Coerce (Coercible, coerce)
import           Data.Foldable
import           Data.MonoTraversable
import           Data.Monoid
import qualified Data.Primitive as Prim
import           Data.Primitive.Addr
import           Data.Primitive.ByteArray
import           Data.Primitive.Types (Prim)
import           Data.Store.Internal
import           Data.Store.TH
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as T
import qualified Data.Text.Unsafe as T
import           Data.Typeable (Typeable)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MGV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Internal as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (peekByteOff, pokeByteOff, Storable, sizeOf)
import           GHC.Generics
import           GHC.Ptr (Ptr(..))
import           GHC.TypeLits
import           Unsafe.Coerce (unsafeCoerce)

class Store a where
    size :: Size a
    poke :: a -> Poke ()
    peek :: Peek a

------------------------------------------------------------------------

combineSize :: forall a b c. (Store a, Store b) => (c -> a) -> (c -> b) -> Size c
combineSize toA toB = combineSize' toA toB size size

encode :: Store a => a -> BS.ByteString
encode x = BS.unsafeCreate
    (getSize size x)
    (\p -> runPoke (poke x) p 0 (\_ _ -> return ()))

decode :: Store a => BS.ByteString -> a
decode (BS.PS x s len) =
    BS.accursedUnutterablePerformIO $ withForeignPtr x $ \p ->
        let total = len + s
            final offset y
                | offset == total = return y
                | offset < total = throwIO $ PeekException offset "Didn't consume all input"
                | otherwise = throwIO $ PeekException offset "Overshot end of buffer"
         in runPeek peek (len + s) p s final

------------------------------------------------------------------------
-- Instances for types based on flat representations

instance Store a => Store (V.Vector a) where
    size = sizeListLike V.length
    poke = pokeListLike V.length
    peek = V.unsafeFreeze =<< peekMutableListLike MV.new MV.write

{-
instance (Prim a, UV.Unbox a) => Store (UV.Vector a) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        Prim.sizeOf (undefined :: a) * UV.length x
    poke x = do
        let PV.Vector offset len array = unboxedToPrimitive x
            !(Addr addr) = byteArrayContents array
            elBytes = Prim.sizeOf (undefined :: a)
        poke len
        pokePtr (Ptr addr) (offset * elBytes) (len * elBytes)
    peek = do
        len <- peek
        Peek $ \total sourcePtr sourceOffset k -> do
            -- TODO: could be cleaned up a little with MGV.unsafeNew?
            let byteLen = Prim.sizeOf (undefined :: a) * len
            marray <- newByteArray byteLen
            let !(Addr addr) = mutableByteArrayContents marray
            BS.memcpy (Ptr addr)
                      (sourcePtr `plusPtr` sourceOffset)
                      byteLen
            array <- unsafeFreezeByteArray marray
            k (sourceOffset + byteLen) (primitiveToUnboxed (PV.Vector 0 len array))

-- I tried (Coercible (UV.Vector a) (PV.Vector a)) in the instance
-- above, but it seems like GHC wants to solve the Coercible constraint
-- right away and can't see that I have a constraint for it. I believe
-- the coercion check would work out once the data family gets applied.

FIXME: commenting these out, because the data rep of UV.Vector () is
_not_ a PV.Vector (). Instead, we need a typeclass and instances for
converting to a primitive vector.

unboxedToPrimitive :: UV.Vector a -> PV.Vector a
unboxedToPrimitive = unsafeCoerce

primitiveToUnboxed :: PV.Vector a -> UV.Vector a
primitiveToUnboxed = unsafeCoerce
-}

instance Storable a => Store (SV.Vector a) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        sizeOf (undefined :: a) * SV.length x
    poke x = do
        let (fptr, len) = SV.unsafeToForeignPtr0 x
        poke len
        pokeForeignPtr fptr 0 (sizeOf (undefined :: a) * len)
    peek = do
        len <- peek
        Peek $ \total sourcePtr sourceOffset k -> do
            mv <- MGV.unsafeNew len
            let (fptr, _) = MSV.unsafeToForeignPtr0 mv
                byteLen = len * sizeOf (undefined :: a)
            withForeignPtr fptr $ \ptr ->
                BS.memcpy (castPtr ptr)
                          (sourcePtr `plusPtr` sourceOffset)
                          byteLen
            x <- SV.unsafeFreeze mv
            k (sourceOffset + byteLen) x

instance Store BS.ByteString where
    size = VarSize BS.length
    poke x = do
        poke (BS.length x)
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        pokeForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        len <- peek
        Peek $ \total sourcePtr sourceOffset k -> do
            x <- BS.create len $ \targetPtr ->
               BS.memcpy targetPtr
                         (sourcePtr `plusPtr` sourceOffset)
                         len
            k (sourceOffset + len) x

instance Store LBS.ByteString where
    -- FIXME: faster conversion? Is this ever going to be a problem?
    --
    -- I think on 64 bit systems, Int will have 64 bits. On 32 bit
    -- systems, we'll never exceed the range of Int by this conversion.
    size = VarSize (fromIntegral . LBS.length)
    poke = poke . LBS.toStrict
    peek = fmap LBS.fromStrict peek

{-
instance Store T.Text where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        2 * (T.lengthWord16 x)
    poke x = do
        let T.Text (TA.Array array w16Len) = x
            !(Addr addr) = byteArrayContents array
        poke w16Len
        pokePtr (Ptr addr) 0 (2 * w16Len)
    peek = do
        w16Len <- peek
        return (T.Text (TA.Array array w16Len))
-}

pokeForeignPtr sourceFp sourceOffset sourceLength =
    Poke $ \targetPtr targetOffset k -> do
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      sourceLength
        k (targetOffset + sourceLength) ()

pokePtr sourcePtr sourceOffset sourceLength =
    Poke $ \targetPtr targetOffset k -> do
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  sourceLength
        k (targetOffset + sourceLength) ()

------------------------------------------------------------------------
-- Utils for implementing 'Store' instances for 'Storable' types.

sizeStorable :: forall a. Storable a => Size a
sizeStorable = ConstSize $ sizeOf
    (error "In Data.Store.storableSize: sizeOf evaluated its argument." :: a)
{-# INLINE sizeStorable #-}

-- | A @Poke@ implementation based on an instance of @Storable@
pokeStorable :: Storable a => a -> Poke ()
pokeStorable x = Poke $ \ptr offset k -> do
    y <- pokeByteOff ptr offset x
    (k $! offset + sizeOf x) y
{-# INLINE pokeStorable #-}

-- FIXME: make it the responsibility of the caller to check this.

-- | A @Peek@ implementation based on an instance of @Storable@
peekStorable :: forall a. Storable a => Peek a
peekStorable = Peek $ \total ptr offset k ->
    let offset' = offset + needed
        needed = sizeOf (undefined :: a)
     in if total >= offset'
            then do
                x <- peekByteOff ptr offset
                k offset' x
            else fail $
                "Attempted to read too many bytes.  Needed " ++
                show needed ++ ", but only " ++
                show (total - offset) ++ " remain."
{-# INLINE peekStorable #-}

------------------------------------------------------------------------
-- Utils for implementing 'Store' instances for list-like Foldable

-- | Implement 'size' for a list-like 'Foldable' of 'Store'
-- instances. This should be preferred to 'listLikeSize'' unless you can
-- ensure that the 'length' implementation is efficient.
--
-- For example, the 'length' implementation is currently inefficient for
-- Vector is currently inefficient:
-- <https://github.com/haskell/vector/issues/112>
sizeListLike :: forall t a. (Foldable t, Store a) => (t a -> Int) -> Size (t a)
sizeListLike len = VarSize $ \t ->
    case size :: Size a of
        ConstSize n -> n * len t + sizeOf (undefined :: Int)
        VarSize f -> foldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeListLike #-}

-- | Implement 'poke' for a list-like 'Foldable' of 'Store'
-- instances. See the docs above for 'listLikeSize' about when this
-- function should be used instead of 'listLikePoke''.
pokeListLike :: (Foldable t, Store a) => (t a -> Int) -> t a -> Poke ()
pokeListLike len t = do
    poke (len t)
    mapM_ poke t
{-# INLINE pokeListLike #-}

-- | Implement 'size' for a list-like 'Foldable' of 'Store' instances.
sizeListLike' :: (Foldable t, Store a) => Size (t a)
sizeListLike' = sizeListLike length
{-# INLINE sizeListLike' #-}

-- | Implement 'poke' for a 'Foldable' of 'Store' instances.
pokeListLike' :: (Foldable t, Store a) => t a -> Poke ()
pokeListLike' = pokeListLike length
{-# INLINE pokeListLike' #-}

-- fromListPeek
--     :: Store a
--     => (Int -> IO r)
--     -> (r -> Int -> a -> IO ())
--     -> Peek r
-- fromListPeek

------------------------------------------------------------------------
-- Util for implementing 'Store' instances for list-like mutable things

peekMutableListLike
    :: Store a
    => (Int -> IO r)
    -> (r -> Int -> a -> IO ())
    -> Peek r
peekMutableListLike new write = do
    n <- peek
    mut <- liftIO (new n)
    forM_ [0..n-1] $ \i -> peek >>= liftIO . write mut i
    return mut
{-# INLINE peekMutableListLike #-}

------------------------------------------------------------------------
-- Utils for implementing 'Store' instances for list-like MonoFoldable

sizeMonoListLike :: forall t. (MonoFoldable t, Store (Element t)) => Size t
sizeMonoListLike = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * olength t + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeMonoListLike #-}

{- Commented out because for most MonoFoldable instances, there are faster impls like memcpy

pokeMonoListLike :: forall t. (MonoFoldable t, Store (Element t)) => t -> Poke ()
pokeMonoListLike t = do
    poke (olength t)
    omapM_ poke t
{-# INLINE pokeMonoListLike #-}
-}

------------------------------------------------------------------------
-- Define a bunch of 'Store' instances for 'Storable' datatypes.

$(deriveManyStoreFromStorable (\_ -> True))
