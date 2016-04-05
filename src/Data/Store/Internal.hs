{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
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
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.Internal
    ( module Data.Store.Impl
    , module Data.Store.Internal
    ) where

import System.IO.Unsafe
import GHC.Ptr
import GHC.Types (IO (..), Int (..))
import           Control.Exception (throwIO, try)
import           Control.Monad.IO.Class (liftIO)
import           Data.Array (Array)
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Containers (IsMap, ContainerKey, MapValue, mapFromList, mapToList, IsSet, setFromList)
import           Data.Foldable (forM_)
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Primitive.ByteArray
import           Data.Primitive.Types (Addr(..))
import           Data.Sequence (Seq)
import           Data.Sequences (IsSequence, Index, replicateM)
import           Data.Set (Set)
import           Data.Store.Impl
import           Data.Store.TH
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Foreign as T
import qualified Data.Text.Internal as T
import           Data.Tree (Tree)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MGV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Ptr (Ptr(..))
import           GHC.Real (Ratio(..))
import           Numeric.Natural (Natural)

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'Foldable'

encode :: Store a => a -> BS.ByteString
encode x = BS.unsafeCreate
    (getSize size x)
    (\p -> runPoke (poke x) p 0 (\_ _ -> return ()))

-- FIXME: can we really justify accursed unutterable things?

decode' :: Peek a -> BS.ByteString -> Either PeekException a
decode' peek' = BS.accursedUnutterablePerformIO . try . decodeImpl' peek'
{-# INLINE decode' #-}

unsafeDecode' :: Peek a -> BS.ByteString -> a
unsafeDecode' peek' = unsafePerformIO . decodeImpl' peek'
{-# INLINE unsafeDecode' #-}

decodeImpl' :: Peek a -> BS.ByteString -> IO a
decodeImpl' peek' (BS.PS x s@(I# s') len) =
    withForeignPtr x $ \(Ptr p) ->
        case len + s of
            I# total -> IO (\s0 ->
                case runPeek peek' total p s' s0 of
                    (# s1, _off, y #) -> (# s1, y #))
{-# INLINE decodeImpl' #-}

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
-- Utilities for defining list-like 'Store' instances in terms of 'isSet'

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

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances for list-like mutable things

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances via memcpy

pokeForeignPtr :: ForeignPtr a -> Int -> Int -> Poke ()
pokeForeignPtr sourceFp sourceOffset sourceLength =
    Poke $ \targetPtr targetOffset k -> do
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      sourceLength
        k (targetOffset + sourceLength) ()

pokePtr :: Ptr a -> Int -> Int -> Poke ()
pokePtr sourcePtr sourceOffset sourceLength =
    Poke $ \targetPtr targetOffset k -> do
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  sourceLength
        k (targetOffset + sourceLength) ()

------------------------------------------------------------------------
-- Instances for types based on flat representations

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
-- instance Store a => Store (Tree a) where

------------------------------------------------------------------------
-- Other instances

-- Manual implementation due to no Generic instance for Store. Also due
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
instance Store a => Store (Dual a)
instance Store a => Store (Sum a)
instance Store a => Store (Product a)
instance Store a => Store (First a)
instance Store a => Store (Last a)
instance Store a => Store (Maybe a)
instance (Store a, Store b) => Store (Either a b)

-- TODO: higher arities?  Limited now by Generics instances for tuples
$(return $ map makeTupleInstance [2..7])

------------------------------------------------------------------------
-- Instances for Storable types

$(deriveManyStoreFromStorable (\_ -> True))
