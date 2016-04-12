{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.Internal
    ( module Data.Store.Impl
    , module Data.Store.Internal
    ) where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Primitive (PrimState)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Coerce (coerce)
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Foldable (forM_)
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Primitive.ByteArray
import           Data.Sequence (Seq)
import           Data.Sequences (IsSequence, Index, replicateM)
import           Data.Set (Set)
import           Data.Store.Impl
import           Data.Store.TH
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Foreign as T
import qualified Data.Text.Internal as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import           Data.Void
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr, castPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Arr (Array(..))
import qualified GHC.Arr as Arr
import           GHC.Prim (copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.Real (Ratio(..))
import           GHC.Types (IO(..), Int(..))

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSequence'

-- | Implement 'size' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
sizeSequence :: forall t. (IsSequence t, Store (Element t)) => Size t
sizeSequence = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSequence #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
pokeSequence :: (IsSequence t, Store (Element t)) => t -> Poke ()
pokeSequence t = do
    pokeStorable (olength t)
    omapM_ poke t
{-# INLINE pokeSequence #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
--
-- Note that many monomorphic containers have more efficient
-- implementations (for example, via memcpy).
peekSequence :: (IsSequence t, Store (Element t), Index t ~ Int) => Peek t
peekSequence = do
    len <- peek
    replicateM len peek
{-# INLINE peekSequence #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'IsSet'

-- | Implement 'size' for an 'IsSet' of 'Store' instances.
sizeSet :: forall t. (IsSet t, Store (Element t)) => Size t
sizeSet = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * (olength t) + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeSet #-}

-- | Implement 'poke' for an 'IsSequence' of 'Store' instances.
pokeSet :: (IsSet t, Store (Element t)) => t -> Poke ()
pokeSet t = do
    pokeStorable (olength t)
    omapM_ poke t
{-# INLINE pokeSet #-}

-- | Implement 'peek' for an 'IsSequence' of 'Store' instances.
peekSet :: (IsSet t, Store (Element t)) => Peek t
peekSet = do
    len <- peek
    setFromList <$> replicateM len peek
{-# INLINE peekSet #-}

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of a 'IsMap'

-- | Implement 'size' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
sizeMap
    :: forall t. (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Size t
sizeMap = VarSize $ \t ->
    case (size :: Size (ContainerKey t), size :: Size (MapValue t)) of
        (ConstSize nk, ConstSize na) -> (nk + na) * olength t + sizeOf (undefined :: Int)
        (szk, sza) -> ofoldl' (\acc (k, a) -> acc + getSize szk k + getSize sza a)
                              (sizeOf (undefined :: Int))
                              (mapToList t)
{-# INLINE sizeMap #-}

-- | Implement 'poke' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
pokeMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => t
    -> Poke ()
pokeMap t = do
    poke (olength t)
    ofoldl' (\acc (k, x) -> poke k >> poke x >> acc)
            (return ())
            (mapToList t)
{-# INLINE pokeMap #-}

-- | Implement 'peek' for an 'IsMap' of where both 'ContainerKey' and
-- 'MapValue' are 'Store' instances.
peekMap
    :: (Store (ContainerKey t), Store (MapValue t), IsMap t)
    => Peek t
peekMap = mapFromList <$> peek
{-# INLINE peekMap #-}

{-
------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of Foldable

-- | Implement 'size' for a 'Foldable' of 'Store' instances. Note that
-- this assumes the extra 'Foldable' structure is discardable - this
-- only serializes the elements.
sizeListLikeFoldable :: forall t a. (Foldable t, Store a) => Size (t a)
sizeListLikeFoldable = VarSize $ \t ->
    case size :: Size e of
        ConstSize n ->  n * length x + sizeOf (undefined :: Int)
        VarSize f -> foldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int))
{-# INLINE sizeSequence #-}

pokeListLikeFoldable :: forall t a. Foldable t => t a -> Poke ()
pokeListLikeFoldable x = do
    poke (length x)
-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances for list-like mutable things

peekMutableSequence
    :: Store a
    => (Int -> IO r)
    -> (r -> Int -> a -> IO ())
    -> Peek r
peekMutableSequence new write = do
    n <- peekStorable
    mut <- liftIO (new n)
    forM_ [0..n-1] $ \i -> peek >>= liftIO . write mut i
    return mut
{-# INLINE peekMutableSequence #-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances via memcpy

pokeForeignPtr :: ForeignPtr a -> Int -> Int -> Poke ()
pokeForeignPtr sourceFp sourceOffset len =
    Poke $ \targetPtr targetOffset k -> do
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      len
        k (targetOffset + len) ()

peekPlainForeignPtr :: String -> Int -> Peek (ForeignPtr a)
peekPlainForeignPtr ty len =
    Peek $ \end sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
        when (ptr2 > end) $
            tooManyBytes len (end `minusPtr` sourcePtr) ty
        fp <- BS.mallocByteString len
        withForeignPtr fp $ \targetPtr ->
            BS.memcpy targetPtr (castPtr sourcePtr) len
        return (ptr2, castForeignPtr fp)

pokePtr :: Ptr a -> Int -> Int -> Poke ()
pokePtr sourcePtr sourceOffset len =
    Poke $ \targetPtr targetOffset k -> do
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  len
        k (targetOffset + len) ()

pokeByteArray :: ByteArray# -> Int -> Int -> Poke ()
pokeByteArray sourceArr sourceOffset len =
    Poke $ \targetPtr targetOffset k -> do
        let target = targetPtr `plusPtr` targetOffset
        copyByteArrayToAddr sourceArr sourceOffset target len
        k (targetOffset + len) ()

peekByteArray :: String -> Int -> Peek ByteArray
peekByteArray ty len =
    Peek $ \end sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
        when (ptr2 > end) $
            tooManyBytes len (end `minusPtr` sourcePtr) ty
        marr <- newByteArray len
        copyAddrToByteArray sourcePtr marr 0 len
        x <- unsafeFreezeByteArray marr
        return (ptr2, x)

copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))

copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))

------------------------------------------------------------------------
-- Instances for types based on flat representations

instance Store a => Store (V.Vector a) where
    size = sizeSequence
    poke = pokeSequence
    peek = V.unsafeFreeze =<< peekMutableSequence MV.new MV.write

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
        fp <- peekPlainForeignPtr "Data.Storable.Vector.Vector" (sizeOf (undefined :: a) * len)
        liftIO $ SV.unsafeFreeze (MSV.MVector len fp)

instance Store BS.ByteString where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        BS.length x
    poke x = do
        poke (BS.length x)
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        pokeForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        len <- peek
        fp <- peekPlainForeignPtr "Data.Storable.Vector.Vector" len
        return (BS.PS fp 0 len)

instance Store SBS.ShortByteString where
    size = VarSize $ \x ->
         sizeOf (undefined :: Int) +
         SBS.length x
    poke x@(SBS.SBS arr) = do
        let len = SBS.length x
        poke len
        pokeByteArray arr 0 len
    peek = do
        len <- peek
        ByteArray array <- peekByteArray "Data.ByteString.Short.ShortByteString" len
        return (SBS.SBS array)

instance Store LBS.ByteString where
    -- FIXME: faster conversion? Is this ever going to be a problem?
    --
    -- I think on 64 bit systems, Int will have 64 bits. On 32 bit
    -- systems, we'll never exceed the range of Int by this conversion.
    size = VarSize $ \x ->
         sizeOf (undefined :: Int)  +
         fromIntegral (LBS.length x)
    -- FIXME: more efficient implementation that avoids the double copy
    poke = poke . LBS.toStrict
    peek = fmap LBS.fromStrict peek

instance Store T.Text where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        2 * (T.lengthWord16 x)
    poke x = do
        let !(T.Text (TA.Array array) w16Off w16Len) = x
        poke w16Len
        pokeByteArray array (2 * w16Off) (2 * w16Len)
    peek = do
        w16Len <- peek
        ByteArray array <- peekByteArray "Data.Text.Text" (2 * w16Len)
        return (T.Text (TA.Array array) 0 w16Len)

{-
-- Gets a little tricky to compute size due to size of storing indices.

instance (Store i, Store e) => Store (Array i e) where
    size = combineSize' () () () $
        VarSize $ \t ->
        case size :: Size e of
            ConstSize n ->  n * length x
            VarSize f -> foldl' (\acc x -> acc + f x) 0
-}

------------------------------------------------------------------------
-- containers instances

instance Store a => Store [a] where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance Store a => Store (Seq a) where
    size = sizeSequence
    poke = pokeSequence
    peek = peekSequence

instance (Store a, Ord a) => Store (Set a) where
    size = sizeSet
    poke = pokeSet
    peek = peekSet

instance Store IntSet where
    size = sizeSet
    poke = pokeSet
    peek = peekSet

instance Store a => Store (IntMap a) where
    size = sizeMap
    poke = pokeMap
    peek = peekMap

instance (Ord k, Store k, Store a) => Store (Map k a) where
    size = sizeMap
    poke = pokeMap
    peek = peekMap

-- FIXME: implement
--
-- instance (Ix i, Bounded i, Store a) => Store (Array ix a) where
--
-- instance (Ix i, Bounded i, Store a) => Store (UA.UArray ix a) where
--
-- instance Store Natural where
--
-- instance Store Integer where
--
-- instance Store GHC.Fingerprint.Types.Fingerprint where
--
-- instance Store a => Store (Fixed a) where
--
-- instance Store a => Store (Tree a) where

------------------------------------------------------------------------
-- Other instances

-- Manual implementation due to no Generic instance for Ratio. Also due
-- to the instance for Storable erroring when the denominator is 0.
-- Perhaps we should keep the behavior but instead a peekException?
--
-- In that case it should also error on poke.
--
-- I prefer being able to Store these, because they are constructable.

instance Store a => Store (Ratio a) where
    size = combineSize (\(x :% _) -> x) (\(_ :% y) -> y)
    poke (x :% y) = poke (x, y)
    peek = uncurry (:%) <$> peek

instance Store ()
instance Store All
instance Store Any
instance Store Void
instance Store a => Store (Dual a)
instance Store a => Store (Sum a)
instance Store a => Store (Product a)
instance Store a => Store (First a)
instance Store a => Store (Last a)
instance Store a => Store (Maybe a)
instance (Store a, Store b) => Store (Either a b)

-- TODO: higher arities?  Limited now by Generics instances for tuples
$(return $ map makeTupleStoreInstance [2..7])

------------------------------------------------------------------------
-- Instances for Storable types

$(deriveManyStoreFromStorable (\_ -> True))
