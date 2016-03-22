{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Store.Generics where

import Data.Proxy (Proxy(..))
import Data.Store
import Data.Store.Internal
import Data.Word (Word8)
import Foreign.Storable (sizeOf)
import GHC.Generics
import GHC.TypeLits

type family SumArity (a :: * -> *) :: Nat where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

class GStore f where
    gsize :: Size (f a)
    gpoke :: f a -> Poke ()
    gpeek :: Peek (f a)

instance GStore f => GStore (M1 i c f) where
    gsize = contramapSize unM1 gsize
    gpoke = gpoke . unM1
    gpeek = fmap M1 gpeek

instance Store a => GStore (K1 i a) where
    gsize = contramapSize unK1 size
    gpoke = poke . unK1
    gpeek = fmap K1 peek

instance GStore U1 where
    gsize = ConstSize 0
    gpoke _ = return ()
    gpeek = return U1

instance (GStore a, GStore b) => GStore (a :*: b) where
    gsize = combineSize' (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
    gpoke (a :*: b) = gpoke a >> gpoke b
    gpeek = (:*:) <$> gpeek <*> gpeek

-- FIXME: check that this type level stuff dosen't get turned into
-- costly runtime computation

instance (SumArity (a :+: b) <= 255, GStoreSum 0 (a :+: b))
         => GStore (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    gpoke x = gpokeSum x (Proxy :: Proxy 0)
    gpeek = do
        tag <- peek
        gpeekSum tag (Proxy :: Proxy 0)

class KnownNat n => GStoreSum (n :: Nat) (f :: * -> *) where
    gsizeSum :: f a -> Proxy n -> Int
    gpokeSum :: f p -> Proxy n -> Poke ()
    gpeekSum :: Word8 -> Proxy n -> Peek (f p)

instance (GStoreSum n a, GStoreSum (n + SumArity a) b, KnownNat n)
         => GStoreSum n (a :+: b) where
    gsizeSum (L1 l) _ = gsizeSum l (Proxy :: Proxy n)
    gsizeSum (R1 r) _ = gsizeSum r (Proxy :: Proxy (n + SumArity a))
    gpokeSum (L1 l) _ = gpokeSum l (Proxy :: Proxy n)
    gpokeSum (R1 r) _ = gpokeSum r (Proxy :: Proxy (n + SumArity a))
    gpeekSum tag proxyL
        | tag < sizeL = L1 <$> gpeekSum tag proxyL
        | otherwise = R1 <$> gpeekSum tag (Proxy :: Proxy (n + SumArity a))
      where
        sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))

instance (GStore a, KnownNat n) => GStoreSum n (C1 c a) where
    gsizeSum x _ = getSize gsize x
    gpokeSum x _ = do
        poke (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
        gpoke x
    gpeekSum tag _
        | tag == cur = gpeek
        | tag < cur = peekException "Error in implementation of Store Generics"
        | tag > cur = peekException "Pointer tag invalid"
      where
        cur = fromInteger (natVal (Proxy :: Proxy n))
