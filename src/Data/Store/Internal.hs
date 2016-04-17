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
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.Internal
    ( module Data.Store.Impl
    , module Data.Store.Internal
    ) where

import           Control.Exception (throwIO)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short.Internal as SBS
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Fixed (Fixed (..), Pico)
import           Data.Foldable (forM_)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)
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
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Base as UV
import           Data.Void
import           Data.Word
import           Foreign.Ptr (plusPtr, minusPtr)
import           Foreign.Storable (Storable, sizeOf)
import qualified GHC.Integer.GMP.Internals as I
import           GHC.Prim (sizeofByteArray#)
import           GHC.Real (Ratio(..))
import           GHC.Types (Int (I#))

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
-- Useful combinators

-- | Skip n bytes forward.
{-# INLINE skip #-}
skip :: Int -> Peek ()
skip len = Peek $ \end ptr -> do
    let ptr2 = ptr `plusPtr` len
    when (ptr2 > end) $
        tooManyBytes len (end `minusPtr` ptr) "skip"
    return (ptr2, ())

-- | Isolate the input to n bytes, skipping n bytes forward. Fails if @m@
-- advances the offset beyond the isolated region.
{-# INLINE isolate #-}
isolate :: Int -> Peek a -> Peek a
isolate len m = Peek $ \end ptr -> do
    let ptr2 = ptr `plusPtr` len
    when (ptr2 > end) $
        tooManyBytes len (end `minusPtr` ptr) "isolate"
    (ptr', x) <- runPeek m end ptr
    when (ptr' > end) $
        throwIO $ PeekException (ptr' `minusPtr` end) "Overshot end of isolated bytes"
    return (ptr2, x)

------------------------------------------------------------------------
-- Instances for types based on flat representations

instance Store a => Store (V.Vector a) where
    size = sizeSequence
    poke = pokeSequence
    peek = V.unsafeFreeze =<< peekMutableSequence MV.new MV.write

-- NOTE: soon we'll have TH generation for all of the unbox instances.
instance Store (UV.Vector Word) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        sizeOf (undefined :: Word) * UV.length x
    poke !(UV.V_Word pv) = poke pv
    peek = UV.V_Word <$> peek

instance Store (UV.Vector Word8) where
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        sizeOf (undefined :: Word8) * UV.length x
    poke !(UV.V_Word8 pv) = poke pv
    peek = UV.V_Word8 <$> peek

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

instance (Eq k, Hashable k, Store k, Store a) => Store (HashMap k a) where
    size = sizeMap
    poke = pokeMap
    peek = peekMap

instance (Eq a, Hashable a, Store a) => Store (HashSet a) where
    size = sizeSet
    poke = pokeSet
    peek = peekSet

-- FIXME: implement
--
-- instance (Ix i, Bounded i, Store a) => Store (Array ix a) where
--
-- instance (Ix i, Bounded i, Store a) => Store (UA.UArray ix a) where
--
-- instance Store Natural where
--
instance Store Integer where
    size = VarSize $ \ x ->
        sizeOf (undefined :: Word8) + case x of
            I.S# _ -> sizeOf (undefined :: Int)
            I.Jp# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
            I.Jn# (I.BN# arr) -> sizeOf (undefined :: Int) + I# (sizeofByteArray# arr)
    poke (I.S# x) = poke (0 :: Word8) >> poke (I# x)
    poke (I.Jp# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (1 :: Word8)
        poke len
        pokeByteArray arr 0 len
    poke (I.Jn# (I.BN# arr)) = do
        let len = I# (sizeofByteArray# arr)
        poke (2 :: Word8)
        poke len
        pokeByteArray arr 0 len
    peek = do
        tag <- peek :: Peek Word8
        case tag of
            0 -> fromIntegral <$> (peek :: Peek Int)
            1 -> I.Jp# <$> peekBN
            2 -> I.Jn# <$> peekBN
            _ -> peekException "Invalid Integer tag"
      where
        peekBN = do
          len <- peek :: Peek Int
          ByteArray arr <- peekByteArray "GHC>Integer" len
          return $ I.BN# arr

--
-- instance Store GHC.Fingerprint.Types.Fingerprint where
--
instance Store (Fixed a) where
    size = contramapSize (\(MkFixed x) -> x) (size :: Size Integer)
    poke (MkFixed x) = poke x
    peek = MkFixed <$> peek

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

instance Store Time.Day where
    size = contramapSize Time.toModifiedJulianDay (size :: Size Integer)
    poke = poke . Time.toModifiedJulianDay
    peek = Time.ModifiedJulianDay <$> peek

instance Store Time.DiffTime where
    size = contramapSize (realToFrac :: Time.DiffTime -> Pico) (size :: Size Pico)
    poke = (poke :: Pico -> Poke ()) . realToFrac
    peek = Time.picosecondsToDiffTime <$> peek

instance Store Time.UTCTime where
    size = combineSize Time.utctDay Time.utctDayTime
    poke (Time.UTCTime day time) = poke (day, time)
    peek = uncurry Time.UTCTime <$> peek

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
$(deriveManyStorePrimVector)
