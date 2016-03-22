{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Store.Internal where

import           Control.Exception (Exception, throwIO)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.Store.TH
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peekByteOff, pokeByteOff, Storable, sizeOf)
import           GHC.Prim ( unsafeCoerce#, RealWorld )
import           GHC.TypeLits

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
    { runPeek :: forall r byte.
        Total
     -> Ptr byte
     -> Offset
     -> (Offset -> a -> IO r)
     -> IO r
    }
    deriving Functor

instance Applicative Peek where
    pure x = Peek (\_ _ offset k -> k offset x)
    {-# INLINE pure #-}
    Peek f <*> Peek g = Peek $ \total ptr offset1 k ->
        f total ptr offset1 $ \offset2 f' ->
        g total ptr offset2 $ \offset3 g' ->
        k offset3 (f' g')
    {-# INLINE (<*>) #-}
    Peek f *> Peek g = Peek $ \total ptr offset1 k ->
        f total ptr offset1 $ \offset2 _ ->
        g total ptr offset2 k
    {-# INLINE (*>) #-}

instance Monad Peek where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Peek x >>= f = Peek $ \total ptr offset1 k ->
        x total ptr offset1 $ \offset2 x' ->
        runPeek (f x') total ptr offset2 k
    {-# INLINE (>>=) #-}
    fail = Fail.fail
    {-# INLINE fail #-}

instance Fail.MonadFail Peek where
    fail = peekException . T.pack
    {-# INLINE fail #-}

instance PrimMonad Peek where
    type PrimState Peek = RealWorld
    primitive action = Peek $ \_ _ offset k -> do
        x <- primitive (unsafeCoerce# action)
        k offset x
    {-# INLINE primitive #-}

instance MonadIO Peek where
    liftIO f = Peek $ \_ _ offset k -> f >>= k offset
    {-# INLINE liftIO #-}

-- | Exception thrown while running 'peek'. Note that other types of
-- exceptions can also be thrown.
data PeekException = PeekException Offset T.Text
    deriving (Eq, Show, Typeable)

instance Exception PeekException

------------------------------------------------------------------------

-- | Info about a type's serialized length. Either the length is known
-- independently of the value, or the length depends on the value.
data Size a
    = VarSize (a -> Int)
    | ConstSize Int
    deriving Typeable

peekException :: T.Text -> Peek a
peekException msg = Peek $ \_ _ offset _ -> throwIO (PeekException offset msg)

-- FIXME: export as utils?

-- TODO: depend on contravariant package? The ConstSize case is a little
-- wonky due to type conversion

contramapSize :: (a -> b) -> Size b -> Size a
contramapSize f (VarSize g) = VarSize (g . f)
contramapSize _ (ConstSize n) = ConstSize n

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
