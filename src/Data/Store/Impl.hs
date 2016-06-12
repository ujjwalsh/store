{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

-- This module is not exposed. The reason that it is split out from
-- "Data.Store.Internal" is to allow "Data.Store.TH" to refer to these
-- identifiers. "Data.Store.Internal" must be separate from
-- "Data.Store.TH" due to Template Haskell's stage restriction.
module Data.Store.Impl where

import           Control.Applicative
import           Control.Exception (Exception(..), throwIO)
import           Control.Exception (try)
import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import           Data.Monoid ((<>))
import           Data.Primitive.ByteArray
import           Data.Proxy
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Ptr (Ptr, plusPtr, minusPtr, castPtr)
import           Foreign.Storable (pokeByteOff, Storable, sizeOf)
import qualified Foreign.Storable as Storable
import           GHC.Generics
import           GHC.Prim (unsafeCoerce#, RealWorld, copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.TypeLits
import           GHC.Types (IO(..), Int(..))
import           Prelude
import           System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------
-- Store class

-- TODO: write down more elaborate laws

-- | The 'Store' typeclass provides efficient serialization and
-- deserialization to raw pointer addresses.
--
-- The 'peek' and 'poke' methods should be defined such that
-- @ decodeEx (encode x) == x @.
class Store a where
    -- | Yields the 'Size' of the buffer, in bytes, required to store
    -- the encoded representation of the type.
    --
    -- Note that the correctness of this function is crucial for the
    -- safety of 'poke', as it does not do any bounds checking. It is
    -- the responsibility of the invoker of 'poke' ('encode' and similar
    -- functions) to ensure that there's enough space in the output
    -- buffer. If 'poke' writes beyond, then arbitrary memory can be
    -- overwritten, causing undefined behavior and segmentation faults.
    size :: Size a
    -- | Serializes a value to bytes. It is the responsibility of the
    -- caller to ensure that at least the number of bytes required by
    -- 'size' are available. These details are handled by 'encode' and
    -- similar utilities.
    poke :: a -> Poke ()
    -- | Serialized a value from bytes, throwing exceptions if it
    -- encounters invalid data or runs out of input bytes.
    peek :: Peek a

    default size :: (Generic a, GStoreSize (Rep a)) => Size a
    size = genericSize
    {-# INLINE size #-}

    default poke :: (Generic a, GStorePoke (Rep a)) => a -> Poke ()
    poke = genericPoke
    {-# INLINE poke #-}

    default peek :: (Generic a , GStorePeek (Rep a)) => Peek a
    peek = genericPeek
    {-# INLINE peek #-}


------------------------------------------------------------------------
-- Utilities for encoding / decoding strict ByteStrings

-- | Serializes a value to a 'BS.ByteString'. In order to do this, it
-- first allocates a 'BS.ByteString' of the correct size (based on
-- 'size'), and then uses 'poke' to fill it.
encode :: Store a => a -> BS.ByteString
encode x = BS.unsafeCreate l $ \p -> do
    (o, ()) <- runPoke (poke x) p 0
    checkOffset o l
  where
    l = getSize x
{-# INLINE encode #-}

checkOffset :: Int -> Int -> IO ()
checkOffset o l
    | o > l = throwIO $ PokeException o $
        "encode overshot end of " <>
        T.pack (show l) <>
        " byte long buffer"
    | o < l = throwIO $ PokeException o $
        "encode undershot end of " <>
        T.pack (show l) <>
        " byte long buffer"
    | otherwise = return ()

-- | Decodes a value from a 'BS.ByteString'. Returns an exception if
-- there's an error while decoding, or if decoding undershoots /
-- overshoots the end of the buffer.
decode :: Store a => BS.ByteString -> Either PeekException a
decode = unsafePerformIO . try . decodeIO
{-# INLINE decode #-}

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeWith :: Peek a -> BS.ByteString -> Either PeekException a
decodeWith mypeek = unsafePerformIO . try . decodeIOWith mypeek
{-# INLINE decodeWith #-}

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeEx :: Store a => BS.ByteString -> a
decodeEx = unsafePerformIO . decodeIO
{-# INLINE decodeEx #-}

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeExWith :: Peek a -> BS.ByteString -> a
decodeExWith f = unsafePerformIO . decodeIOWith f
{-# INLINE decodeExWith #-}

-- | Similar to 'decodeExWith', but it allows there to be more of the
-- buffer remaining. The 'Offset' of the buffer contents immediately
-- after the decoded value is returned.
decodeExPortionWith :: Peek a -> BS.ByteString -> (Offset, a)
decodeExPortionWith f = unsafePerformIO . decodeIOPortionWith f
{-# INLINE decodeExPortionWith #-}

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeIO :: Store a => BS.ByteString -> IO a
decodeIO = decodeIOWith peek
{-# INLINE decodeIO #-}

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeIOWith :: Peek a -> BS.ByteString -> IO a
decodeIOWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOWithFromPtr mypeek ptr len
{-# INLINE decodeIOWith #-}

-- | Similar to 'decodeExPortionWith', but runs in the 'IO' monad.
decodeIOPortionWith :: Peek a -> BS.ByteString -> IO (Offset, a)
decodeIOPortionWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOPortionWithFromPtr mypeek ptr len
{-# INLINE decodeIOPortionWith #-}

-- | Like 'decodeIOWith', but using 'Ptr' and length instead of a 'ByteString'.
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
-- Generics

genericSize :: (Generic a, GStoreSize (Rep a)) => Size a
genericSize = contramapSize from gsize
{-# INLINE genericSize #-}

genericPoke :: (Generic a, GStorePoke (Rep a)) => a -> Poke ()
genericPoke = gpoke . from
{-# INLINE genericPoke #-}

genericPeek :: (Generic a , GStorePeek (Rep a)) => Peek a
genericPeek = to <$> gpeek
{-# INLINE genericPeek #-}

type family SumArity (a :: * -> *) :: Nat where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

-- This could be just one typeclass, but currently compile times are
-- better with things split up.
-- https://github.com/bos/aeson/pull/335
--

class GStoreSize f where gsize :: Size (f a)
class GStorePoke f where gpoke :: f a -> Poke ()
class GStorePeek f where gpeek :: Peek (f a)

instance GStoreSize f => GStoreSize (M1 i c f) where
    gsize = contramapSize unM1 gsize
    {-# INLINE gsize #-}
instance GStorePoke f => GStorePoke (M1 i c f) where
    gpoke = gpoke . unM1
    {-# INLINE gpoke #-}
instance GStorePeek f => GStorePeek (M1 i c f) where
    gpeek = fmap M1 gpeek
    {-# INLINE gpeek #-}

instance Store a => GStoreSize (K1 i a) where
    gsize = contramapSize unK1 size
    {-# INLINE gsize #-}
instance Store a => GStorePoke (K1 i a) where
    gpoke = poke . unK1
    {-# INLINE gpoke #-}
instance Store a => GStorePeek (K1 i a) where
    gpeek = fmap K1 peek
    {-# INLINE gpeek #-}

instance GStoreSize U1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
instance GStorePoke U1 where
    gpoke _ = return ()
    {-# INLINE gpoke #-}
instance GStorePeek U1 where
    gpeek = return U1
    {-# INLINE gpeek #-}

instance GStoreSize V1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
instance GStorePoke V1 where
    gpoke x = case x of {}
    {-# INLINE gpoke #-}
instance GStorePeek V1 where
    gpeek = undefined
    {-# INLINE gpeek #-}

instance (GStoreSize a, GStoreSize b) => GStoreSize (a :*: b) where
    gsize = combineSize' (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
    {-# INLINE gsize #-}
instance (GStorePoke a, GStorePoke b) => GStorePoke (a :*: b) where
    gpoke (a :*: b) = gpoke a >> gpoke b
    {-# INLINE gpoke #-}
instance (GStorePeek a, GStorePeek b) => GStorePeek (a :*: b) where
    gpeek = (:*:) <$> gpeek <*> gpeek
    {-# INLINE gpeek #-}

-- The machinery for sum types is why UndecidableInstances is necessary.

-- FIXME: check that this type level stuff dosen't get turned into
-- costly runtime computation

instance (SumArity (a :+: b) <= 255, GStoreSizeSum 0 (a :+: b))
         => GStoreSize (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}
instance (SumArity (a :+: b) <= 255, GStorePokeSum 0 (a :+: b))
         => GStorePoke (a :+: b) where
    gpoke x = gpokeSum x (Proxy :: Proxy 0)
    {-# INLINE gpoke #-}
instance (SumArity (a :+: b) <= 255, GStorePeekSum 0 (a :+: b))
         => GStorePeek (a :+: b) where
    gpeek = do
        tag <- peekStorable
        gpeekSum tag (Proxy :: Proxy 0)
    {-# INLINE gpeek #-}

-- Similarly to splitting up the generic class into multiple classes, we
-- also split up the one for sum types.

class KnownNat n => GStoreSizeSum (n :: Nat) (f :: * -> *) where gsizeSum :: f a -> Proxy n -> Int
class KnownNat n => GStorePokeSum (n :: Nat) (f :: * -> *) where gpokeSum :: f p -> Proxy n -> Poke ()
class KnownNat n => GStorePeekSum (n :: Nat) (f :: * -> *) where gpeekSum :: Word8 -> Proxy n -> Peek (f p)

instance (GStoreSizeSum n a, GStoreSizeSum (n + SumArity a) b, KnownNat n)
         => GStoreSizeSum n (a :+: b) where
    gsizeSum (L1 l) _ = gsizeSum l (Proxy :: Proxy n)
    gsizeSum (R1 r) _ = gsizeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gsizeSum #-}
instance (GStorePokeSum n a, GStorePokeSum (n + SumArity a) b, KnownNat n)
         => GStorePokeSum n (a :+: b) where
    gpokeSum (L1 l) _ = gpokeSum l (Proxy :: Proxy n)
    gpokeSum (R1 r) _ = gpokeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gpokeSum #-}
instance (GStorePeekSum n a, GStorePeekSum (n + SumArity a) b, KnownNat n)
         => GStorePeekSum n (a :+: b) where
    gpeekSum tag proxyL
        | tag < sizeL = L1 <$> gpeekSum tag proxyL
        | otherwise = R1 <$> gpeekSum tag (Proxy :: Proxy (n + SumArity a))
      where
        sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
    {-# INLINE gpeekSum #-}

instance (GStoreSize a, KnownNat n) => GStoreSizeSum n (C1 c a) where
    gsizeSum x _ = getSizeWith gsize x
    {-# INLINE gsizeSum #-}
instance (GStorePoke a, KnownNat n) => GStorePokeSum n (C1 c a) where
    gpokeSum x _ = do
        pokeStorable (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
        gpoke x
    {-# INLINE gpokeSum #-}
instance (GStorePeek a, KnownNat n) => GStorePeekSum n (C1 c a) where
    gpeekSum tag _
        | tag == cur = gpeek
        | tag > cur = peekException "Sum tag invalid"
        | otherwise = peekException "Error in implementation of Store Generics"
      where
        cur = fromInteger (natVal (Proxy :: Proxy n))
    {-# INLINE gpeekSum #-}

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
-- Helpful Type Synonyms

-- | Total byte size of the given Ptr
type Total = Int

-- | How far into the given Ptr to look
type Offset = Int

------------------------------------------------------------------------
-- Poke monad

newtype Poke a = Poke
    { runPoke :: forall byte. Ptr byte -> Offset -> IO (Offset, a)
      -- ^ Run the 'Poke' action, with the 'Ptr' to the buffer where
      -- data is poked, and the current 'Offset'. The result is the new
      -- offset, along with a return value.
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

pokeException :: T.Text -> Poke a
pokeException msg = Poke $ \_ off -> throwIO (PokeException off msg)

------------------------------------------------------------------------
-- Peek monad

newtype Peek a = Peek
    { runPeek :: forall byte. Ptr byte -> Ptr byte -> IO (Ptr byte, a)
      -- ^ Run the 'Peek' action, with a 'Ptr' to the end of the buffer
      -- where data is poked, and a 'Ptr' to the current position. The
      -- result is the 'Ptr', along with a return value.
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

peekException :: T.Text -> Peek a
peekException msg = Peek $ \end ptr -> throwIO (PeekException (end `minusPtr` ptr) msg)

tooManyBytes :: Int -> Int -> String -> IO void
tooManyBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read too many bytes for " ++
        ty ++
        ". Needed " ++
        show needed ++ ", but only " ++
        show remaining ++ " remain."

------------------------------------------------------------------------
-- Size type

-- | Info about a type's serialized length. Either the length is known
-- independently of the value, or the length depends on the value.
data Size a
    = VarSize (a -> Int)
    | ConstSize !Int
    deriving Typeable

getSize :: Store a => a -> Int
getSize = getSizeWith size
{-# INLINE getSize #-}

getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n
{-# INLINE getSizeWith #-}

-- TODO: depend on contravariant package? The ConstSize case is a little
-- wonky due to type conversion

contramapSize :: (a -> b) -> Size b -> Size a
contramapSize f (VarSize g) = VarSize (g . f)
contramapSize _ (ConstSize n) = ConstSize n
{-# INLINE contramapSize #-}

combineSize :: forall a b c. (Store a, Store b) => (c -> a) -> (c -> b) -> Size c
combineSize toA toB = combineSize' toA toB size size
{-# INLINE combineSize #-}

combineSize' :: forall a b c. (c -> a) -> (c -> b) -> Size a -> Size b -> Size c
combineSize' toA toB sizeA sizeB =
    case (sizeA, sizeB) of
        (VarSize f, VarSize g) -> VarSize (\x -> f (toA x) + g (toB x))
        (VarSize f, ConstSize m) -> VarSize (\x -> f (toA x) + m)
        (ConstSize n, VarSize g) -> VarSize (\x -> n + g (toB x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE combineSize' #-}

scaleSize :: Int -> Size a -> Size a
scaleSize s (ConstSize n) = ConstSize (s * n)
scaleSize s (VarSize f) = VarSize ((s *) . f)
{-# INLINE scaleSize #-}

addSize :: Int -> Size a -> Size a
addSize x (ConstSize n) = ConstSize (x + n)
addSize x (VarSize f) = VarSize ((x +) . f)
{-# INLINE addSize #-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances via memcpy

pokeFromForeignPtr :: ForeignPtr a -> Int -> Int -> Poke ()
pokeFromForeignPtr sourceFp sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      len
        let !newOffset = targetOffset + len
        return (newOffset, ())

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

pokeFromPtr :: Ptr a -> Int -> Int -> Poke ()
pokeFromPtr sourcePtr sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  len
        let !newOffset = targetOffset + len
        return (newOffset, ())

pokeFromByteArray :: ByteArray# -> Int -> Int -> Poke ()
pokeFromByteArray sourceArr sourceOffset len =
    Poke $ \targetPtr targetOffset -> do
        let target = targetPtr `plusPtr` targetOffset
        copyByteArrayToAddr sourceArr sourceOffset target len
        let !newOffset = targetOffset + len
        return (newOffset, ())

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

copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))

copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
