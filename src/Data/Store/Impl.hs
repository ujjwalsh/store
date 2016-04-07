{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Store.Impl where

import           Control.Exception (Exception, throwIO)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.Proxy
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word
import           Foreign.Ptr (Ptr, plusPtr, castPtr)
import           Foreign.Storable (pokeByteOff, Storable, sizeOf)
import qualified Foreign.Storable as Storable
import           GHC.Generics
import           GHC.Prim ( unsafeCoerce#, RealWorld, State#, Addr#, plusAddr#, gtAddr# )
import           GHC.TypeLits
import           GHC.Types (IO (..), Int (..), isTrue#)
import           GHC.Ptr (Ptr (..))

------------------------------------------------------------------------
-- Store class

class Store a where
    size :: Size a
    poke :: a -> Poke ()
    peek :: Peek a

    default size :: (Generic a, GStore (Rep a)) => Size a
    size = contramapSize from gsize

    default poke :: (Generic a, GStore (Rep a)) => a -> Poke ()
    poke = gpoke . from

    default peek :: (Generic a , GStore (Rep a)) => Peek a
    peek = to <$> gpeek

------------------------------------------------------------------------
-- Generics instances

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

-- The machinery for sum types is why UndecidableInstances is necessary.

-- FIXME: check that this type level stuff dosen't get turned into
-- costly runtime computation

instance (SumArity (a :+: b) <= 255, GStoreSum 0 (a :+: b))
         => GStore (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    gpoke x = gpokeSum x (Proxy :: Proxy 0)
    gpeek = do
        tag <- peekStorable
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
        pokeStorable (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
        gpoke x
    gpeekSum tag _
        | tag == cur = gpeek
        | tag > cur = peekException "Pointer tag invalid"
        | otherwise = peekException "Error in implementation of Store Generics"
      where
        cur = fromInteger (natVal (Proxy :: Proxy n))

------------------------------------------------------------------------
-- Utilities for defining 'Store' instances based on 'Storable'

sizeStorable :: forall a. (Storable a, Typeable a) => Size a
sizeStorable = ConstSize (sizeOf (error msg :: a))
  where
    msg =
        "In Data.Store.storableSize: " ++
        show (typeRep (Proxy :: Proxy a)) ++
        "'s sizeOf evaluated its argument."
{-# INLINE sizeStorable #-}

-- | A @Poke@ implementation based on an instance of @Storable@
pokeStorable :: Storable a => a -> Poke ()
pokeStorable x = Poke $ \ptr offset k -> do
    y <- pokeByteOff ptr offset x
    (k $! offset + sizeOf x) y
{-# INLINE pokeStorable #-}

-- FIXME: make it the responsibility of the caller to check this.

-- | A @Peek@ implementation based on an instance of @Storable@
peekStorable :: forall a. (Storable a, Typeable a) => Peek a
peekStorable = Peek $ \end ptr s ->
    let ptr' = plusAddr# ptr needed
        !(I# needed) = sizeOf (undefined :: a)
     in if isTrue# (gtAddr# end ptr')
            then let IO f = Storable.peek (Ptr ptr)
                  in case f s of
                       (# s', x #) -> (# s', ptr', x #)
            else error "FIXME peekStorable"
            {- throwIO $ PeekException 0 {-offset-} $ T.pack $
                "Attempted to read too many bytes for " ++
                -- FIXME
                ""
                {-
                show (typeRep (Proxy :: Proxy a)) ++
                ". Needed " ++
                show needed ++ ", but only " ++
                show (total - offset) ++ " remain. Total: " ++ show total
                -}
                -}
{-# INLINE peekStorable #-}

------------------------------------------------------------------------
-- Helpful Type Synonyms

-- | Total byte size of the given Ptr
type Total = Int

-- | How far into the given Ptr to look
type Offset = Int

------------------------------------------------------------------------
-- Poke monad

newtype Poke a = Poke
    { runPoke :: forall byte r.
        Ptr byte
     -> Offset
     -> (Offset -> a -> IO r)
     -> IO r
    }
    deriving Functor

instance Applicative Poke where
    pure x = Poke $ \_ offset k -> k offset x
    {-# INLINE pure #-}
    Poke f <*> Poke g = Poke $ \ptr offset1 k ->
        f ptr offset1 $ \offset2 f' ->
        g ptr offset2 $ \offset3 g' ->
        k offset3 (f' g')
    {-# INLINE (<*>) #-}
    Poke f *> Poke g = Poke $ \ptr offset1 k ->
        f ptr offset1 $ \offset2 _ ->
        g ptr offset2 $ \offset3 g' ->
        k offset3 g'
    {-# INLINE (*>) #-}

instance Monad Poke where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Poke x >>= f = Poke $ \ptr offset1 k ->
        x ptr offset1 $ \offset2 x' ->
        runPoke (f x') ptr offset2 k
    {-# INLINE (>>=) #-}

instance MonadIO Poke where
    liftIO f = Poke $ \_ offset k -> f >>= k offset
    {-# INLINE liftIO #-}

------------------------------------------------------------------------
-- Peek monad

newtype Peek a = Peek
    { runPeek ::
        Addr# -- one byte past end of buffer
     -> Addr#
     -> State# RealWorld
     -> (# State# RealWorld, Addr#, a #)
    }
   deriving Functor

instance Applicative Peek where
    pure x = Peek (\_ ptr s -> (# s, ptr, x #))
    {-# INLINE pure #-}
    Peek f <*> Peek g = Peek $ \end ptr1 s1 ->
      case f end ptr1 s1 of
        (# s2, ptr2, f' #) ->
          case g end ptr2 s2 of
            (# s3, ptr3, g' #) ->
              (# s3, ptr3, f' g' #)
    {-# INLINE (<*>) #-}
    Peek f *> Peek g = Peek $ \end ptr1 s1 ->
        case f end ptr1 s1 of
          (# s2, ptr2, _f' #) -> g end ptr2 s2
    {-# INLINE (*>) #-}

instance Monad Peek where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Peek x >>= f = Peek $ \end ptr1 s1 ->
      case x end ptr1 s1 of
        (# s2, ptr2, x' #) ->
          runPeek (f x') end ptr2 s2
    {-# INLINE (>>=) #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Peek where
    fail = peekException . T.pack
    {-# INLINE fail #-}

instance PrimMonad Peek where
    type PrimState Peek = RealWorld
    primitive action = Peek $ \_ ptr s ->
        case action s of
            (# s', x #) -> (# s, ptr, x #)
    {-# INLINE primitive #-}

instance MonadIO Peek where
    liftIO (IO f) = Peek $ \_ ptr s ->
        case f s of
            (# s', x #) -> (# s', ptr, x #)
    {-# INLINE liftIO #-}

-- | Exception thrown while running 'peek'. Note that other types of
-- exceptions can also be thrown.
data PeekException = PeekException Offset T.Text
    deriving (Eq, Show, Typeable)

instance Exception PeekException

------------------------------------------------------------------------
-- Size type

-- | Info about a type's serialized length. Either the length is known
-- independently of the value, or the length depends on the value.
data Size a
    = VarSize (a -> Int)
    | ConstSize Int
    deriving Typeable

peekException :: T.Text -> Peek a
peekException msg = error "FIXME peekException" -- Peek $ \_ _ -> throwIO (PeekException offset msg)

-- FIXME: export as utils?

-- TODO: depend on contravariant package? The ConstSize case is a little
-- wonky due to type conversion

contramapSize :: (a -> b) -> Size b -> Size a
contramapSize f (VarSize g) = VarSize (g . f)
contramapSize _ (ConstSize n) = ConstSize n

combineSize :: forall a b c. (Store a, Store b) => (c -> a) -> (c -> b) -> Size c
combineSize toA toB = combineSize' toA toB size size

combineSize' :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
combineSize' toA toB sizeA sizeB =
    case (sizeA, sizeB) of
        (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
        (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
        (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)

getSize :: Size a -> a -> Int
getSize (VarSize f) x = f x
getSize (ConstSize n) _ = n
