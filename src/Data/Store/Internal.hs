{-# LANGUAGE CPP #-}
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
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.Internal
    ( module Data.Store.Impl
    , module Data.Store.Internal
    ) where

import           Data.Store.Impl

import           Control.Exception (throwIO, try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable
import           Data.Store.TH
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MGV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as MSV
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (Storable, sizeOf)
import           GHC.Real (Ratio(..))
import           Language.Haskell.TH

------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'Foldable'

encode :: Store a => a -> BS.ByteString
encode x = BS.unsafeCreate
    (getSize size x)
    (\p -> runPoke (poke x) p 0 (\_ _ -> return ()))

-- FIXME: can we really justify accursed unutterable things?

decode :: Store a => BS.ByteString -> Either PeekException a
decode = BS.accursedUnutterablePerformIO . try . decodeImpl

unsafeDecode :: Store a => BS.ByteString -> a
unsafeDecode = BS.accursedUnutterablePerformIO . decodeImpl

decodeImpl :: Store a => BS.ByteString -> IO a
decodeImpl (BS.PS x s len) =
    withForeignPtr x $ \p ->
        let total = len + s
            final offset y
                | offset == total = return y
                | offset < total = throwIO $ PeekException offset "Didn't consume all input"
                | otherwise = throwIO $ PeekException offset "Overshot end of buffer"
         in runPeek peek (len + s) p s final


------------------------------------------------------------------------
-- Utilities for defining list-like 'Store' instances in terms of 'Foldable'

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
    pokeStorable (len t)
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
-- Utilities for implementing 'Store' instances for list-like mutable things

peekMutableListLike
    :: Store a
    => (Int -> IO r)
    -> (r -> Int -> a -> IO ())
    -> Peek r
peekMutableListLike new write = do
    n <- peekStorable
    mut <- liftIO (new n)
    forM_ [0..n-1] $ \i -> peek >>= liftIO . write mut i
    return mut
{-# INLINE peekMutableListLike #-}

{- Commented out because for most MonoFoldable instances, there are faster impls like memcpy

------------------------------------------------------------------------
-- Utils for implementing 'Store' instances for list-like MonoFoldable

sizeMonoListLike :: forall t. (MonoFoldable t, Store (Element t)) => Size t
sizeMonoListLike = VarSize $ \t ->
    case size :: Size (Element t) of
        ConstSize n -> n * olength t + sizeOf (undefined :: Int)
        VarSize f -> ofoldl' (\acc x -> acc + f x) (sizeOf (undefined :: Int)) t
{-# INLINE sizeMonoListLike #-}


pokeMonoListLike :: forall t. (MonoFoldable t, Store (Element t)) => t -> Poke ()
pokeMonoListLike t = do
    poke (olength t)
    omapM_ poke t
{-# INLINE pokeMonoListLike #-}

-}

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
        Peek $ \_ sourcePtr sourceOffset k -> do
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
    size = VarSize $ \x ->
        sizeOf (undefined :: Int) +
        BS.length x
    poke x = do
        poke (BS.length x)
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        pokeForeignPtr sourceFp sourceOffset sourceLength
    peek = do
        len <- peek
        Peek $ \_ sourcePtr sourceOffset k -> do
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
    size = VarSize $ \x ->
         sizeOf (undefined :: Int)  +
         fromIntegral (LBS.length x)
    -- FIXME: more efficient implementation that avoids the double copy
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

$(do let mkTupleInstance n =
             instanceD (mapM (appT (conT ''Store)) qtvs)
                       (appT (conT ''Store) (appsT (tupleT n : qtvs)))
                       []
           where
             qtvs = take n (map (varT . mkName . (:[])) ['a'..'z'])
     -- TODO: higher arities?  Limited now by Generics instances for tuples
     mapM mkTupleInstance [2..7])

------------------------------------------------------------------------
-- Instances for Storable types

$(deriveManyStoreFromStorable (\_ -> True))
