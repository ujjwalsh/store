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
import           GHC.Prim ( unsafeCoerce#, RealWorld )
import           GHC.Prim (copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.TypeLits
import           GHC.Types (IO(..), Int(..))
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

    default size :: (Generic a, GStore (Rep a)) => Size a
    size = contramapSize from gsize

    default poke :: (Generic a, GStore (Rep a)) => a -> Poke ()
    poke = genericPoke

    default peek :: (Generic a , GStore (Rep a)) => Peek a
    peek = genericPeek

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

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeWith :: Peek a -> BS.ByteString -> Either PeekException a
decodeWith mypeek = unsafePerformIO . try . decodeIOWith mypeek

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeEx :: Store a => BS.ByteString -> a
decodeEx = unsafePerformIO . decodeIO

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeExWith :: Peek a -> BS.ByteString -> a
decodeExWith f = unsafePerformIO . decodeIOWith f

-- | Similar to 'decodeExWith', but it allows there to be more of the
-- buffer remaining. The 'Offset' of the buffer contents immediately
-- after the decoded value is returned.
decodeExPortionWith :: Peek a -> BS.ByteString -> (Offset, a)
decodeExPortionWith f = unsafePerformIO . decodeIOPortionWith f

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions. It is an exception to not consume all input.
decodeIO :: Store a => BS.ByteString -> IO a
decodeIO = decodeIOWith peek

-- | Decodes a value from a 'BS.ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeIOWith :: Peek a -> BS.ByteString -> IO a
decodeIOWith mypeek bs = do
    (offset, x) <- decodeIOPortionWith mypeek bs
    let remaining = BS.length bs - offset
    if remaining > 0
        then throwIO $ PeekException remaining "Didn't consume all input."
        else return x

-- |
decodeIOPortionWith :: Peek a -> BS.ByteString -> IO (Offset, a)
decodeIOPortionWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
            end = ptr `plusPtr` len
         in do
             (ptr2, x') <- runPeek mypeek end ptr
             if ptr2 > end
                 then throwIO $ PeekException (end `minusPtr` ptr2) "Overshot end of buffer"
                 else return (ptr2 `minusPtr` ptr, x')

------------------------------------------------------------------------
-- Generics

genericPoke :: (Generic a, GStore (Rep a)) => a -> Poke ()
genericPoke = gpoke . from
{-# INLINE genericPoke #-}

genericPeek :: (Generic a , GStore (Rep a)) => Peek a
genericPeek = to <$> gpeek
{-# INLINE genericPeek #-}

type family SumArity (a :: * -> *) :: Nat where
    SumArity (C1 c a) = 1
    SumArity (x :+: y) = SumArity x + SumArity y

class GStore f where
    gsize :: Size (f a)
    gpoke :: f a -> Poke ()
    gpeek :: Peek (f a)

instance GStore f => GStore (M1 i c f) where
    gsize = contramapSize unM1 gsize
    {-# INLINE gsize #-}
    gpoke = gpoke . unM1
    {-# INLINE gpoke #-}
    gpeek = fmap M1 gpeek
    {-# INLINE gpeek #-}

instance Store a => GStore (K1 i a) where
    gsize = contramapSize unK1 size
    {-# INLINE gsize #-}
    gpoke = poke . unK1
    {-# INLINE gpoke #-}
    gpeek = fmap K1 peek
    {-# INLINE gpeek #-}

instance GStore U1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
    gpoke _ = return ()
    {-# INLINE gpoke #-}
    gpeek = return U1
    {-# INLINE gpeek #-}

instance GStore V1 where
    gsize = ConstSize 0
    {-# INLINE gsize #-}
    gpoke x = case x of {}
    {-# INLINE gpoke #-}
    gpeek = undefined
    {-# INLINE gpeek #-}

instance (GStore a, GStore b) => GStore (a :*: b) where
    gsize = combineSize' (\(x :*: _) -> x) (\(_ :*: y) -> y) gsize gsize
    {-# INLINE gsize #-}
    gpoke (a :*: b) = gpoke a >> gpoke b
    {-# INLINE gpoke #-}
    gpeek = (:*:) <$> gpeek <*> gpeek
    {-# INLINE gpeek #-}

-- The machinery for sum types is why UndecidableInstances is necessary.

-- FIXME: check that this type level stuff dosen't get turned into
-- costly runtime computation

instance (SumArity (a :+: b) <= 255, GStoreSum 0 (a :+: b))
         => GStore (a :+: b) where
    gsize = VarSize $ \x -> sizeOf (undefined :: Word8) + gsizeSum x (Proxy :: Proxy 0)
    {-# INLINE gsize #-}
    gpoke x = gpokeSum x (Proxy :: Proxy 0)
    {-# INLINE gpoke #-}
    gpeek = do
        tag <- peekStorable
        gpeekSum tag (Proxy :: Proxy 0)
    {-# INLINE gpeek #-}

class KnownNat n => GStoreSum (n :: Nat) (f :: * -> *) where
    gsizeSum :: f a -> Proxy n -> Int
    gpokeSum :: f p -> Proxy n -> Poke ()
    gpeekSum :: Word8 -> Proxy n -> Peek (f p)

instance (GStoreSum n a, GStoreSum (n + SumArity a) b, KnownNat n)
         => GStoreSum n (a :+: b) where
    gsizeSum (L1 l) _ = gsizeSum l (Proxy :: Proxy n)
    gsizeSum (R1 r) _ = gsizeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gsizeSum #-}
    gpokeSum (L1 l) _ = gpokeSum l (Proxy :: Proxy n)
    gpokeSum (R1 r) _ = gpokeSum r (Proxy :: Proxy (n + SumArity a))
    {-# INLINE gpokeSum #-}
    gpeekSum tag proxyL
        | tag < sizeL = L1 <$> gpeekSum tag proxyL
        | otherwise = R1 <$> gpeekSum tag (Proxy :: Proxy (n + SumArity a))
      where
        sizeL = fromInteger (natVal (Proxy :: Proxy (n + SumArity a)))
    {-# INLINE gpeekSum #-}

instance (GStore a, KnownNat n) => GStoreSum n (C1 c a) where
    gsizeSum x _ = getSizeWith gsize x
    {-# INLINE gsizeSum #-}
    gpokeSum x _ = do
        pokeStorable (fromInteger (natVal (Proxy :: Proxy n)) :: Word8)
        gpoke x
    {-# INLINE gpokeSum #-}
    gpeekSum tag _
        | tag == cur = gpeek
        | tag > cur = peekException "Sum tag invalid"
        | otherwise = peekException "Error in implementation of Store Generics"
      where
        cur = fromInteger (natVal (Proxy :: Proxy n))
    {-# INLINE gpeekSum #-}

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
pokeStorable x = Poke $ \ptr offset -> do
    y <- pokeByteOff ptr offset x
    let !newOffset = offset + sizeOf x
    return (newOffset, y)
{-# INLINE pokeStorable #-}

-- FIXME: make it the responsibility of the caller to check this.

-- | A @Peek@ implementation based on an instance of @Storable@
peekStorable :: forall a. (Storable a, Typeable a) => Peek a
peekStorable = Peek $ \end ptr ->
    let ptr' = ptr `plusPtr` needed
        needed = sizeOf (undefined :: a)
        remaining = end `minusPtr` ptr
     in do
        when (ptr' > end) $
            tooManyBytes needed remaining (show (typeRep (Proxy :: Proxy a)))
        x <- Storable.peek (castPtr ptr)
        return (ptr', x)
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
    { runPoke :: forall byte.
        Ptr byte -- ^ `Ptr` to which data is poked.
     -> Offset   -- ^ `Offset` before this poke.
     -> IO (Offset, a) -- ^ Pass around `Offset` after the poke.
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
    displayException (PokeException offset msg) =
        "Exception while poking, at byte index " ++
        show offset ++
        " : " ++
        T.unpack msg

pokeException :: T.Text -> Poke a
pokeException msg = Poke $ \_ off -> throwIO (PokeException off msg)

------------------------------------------------------------------------
-- Peek monad

newtype Peek a = Peek
    { runPeek :: forall byte.
        Ptr byte
     -> Ptr byte
     -> IO (Ptr byte, a)
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
    displayException (PeekException offset msg) =
        "Exception while peeking, " ++
        show offset ++
        " bytes from end: " ++
        T.unpack msg

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

getSizeWith :: Size a -> a -> Int
getSizeWith (VarSize f) x = f x
getSizeWith (ConstSize n) _ = n

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

scaleSize :: Int -> Size a -> Size a
scaleSize s (ConstSize n) = ConstSize (s * n)
scaleSize s (VarSize f) = VarSize ((s *) . f)

addSize :: Int -> Size a -> Size a
addSize x (ConstSize n) = ConstSize (x + n)
addSize x (VarSize f) = VarSize ((x +) . f)

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
