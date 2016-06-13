{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Store.Core.Internal where

import           Control.Applicative
import           Control.Exception (Exception(..), throwIO)
import qualified Control.Monad.Fail as Fail
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.Monoid ((<>))
import           Data.Primitive.ByteArray
import qualified Data.Text as T
import           Data.Typeable
import           Foreign.Ptr (minusPtr)
import           GHC.Prim (unsafeCoerce#, RealWorld, copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.Types (IO(..), Int(..))
import           Prelude

------------------------------------------------------------------------
-- Utilities un-exported by Data.Store.Core

-- | Checks if the offset matches the expected length, and throw a
-- 'PokeException' otherwise.
checkOffset :: Int -> Int -> IO ()
checkOffset o l
    | o > l = throwIO $ PokeException o $ T.pack $
        "encode overshot end of " ++
        show l ++
        " byte long buffer"
    | o < l = throwIO $ PokeException o $ T.pack $
        "encode undershot end of " <>
        show l <>
        " byte long buffer"
    | otherwise = return ()

-- | Throws a 'PeekException' about an attempt to read too many bytes.
tooManyBytes :: Int -> Int -> String -> IO void
tooManyBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read too many bytes for " ++
        ty ++
        ". Needed " ++
        show needed ++ ", but only " ++
        show remaining ++ " remain."

-- | Wrapper around @copyByteArrayToAddr#@ primop.
copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))

-- | Wrapper around @copyAddrToByteArray#@ primop.
copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))

------------------------------------------------------------------------
-- Helpful Type Synonyms

-- | How far into the given Ptr to look
type Offset = Int

------------------------------------------------------------------------
-- Poke monad

-- | 'Poke' actions are useful for building sequential serializers.
--
-- They are actions which write values to bytes into memory specified by
-- a 'Ptr' base. The 'Applicative' and 'Monad' instances make it easy to
-- write serializations, by keeping track of the 'Offset' of the current
-- byte. They allow you to chain 'Poke' action such that subsequent
-- 'Poke's write into subsequent portions of the output.
newtype Poke a = Poke
    { runPoke :: forall byte. Ptr byte -> Offset -> IO (Offset, a)
      -- ^ Run the 'Poke' action, with the 'Ptr' to the buffer where
      -- data is poked, and the current 'Offset'. The result is the new
      -- offset, along with a return value.
      --
      -- May throw a 'PokeException', though this should be avoided when
      -- possible.  They usually indicate a programming error.
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

-- | Throws a 'PokeException'. These should be avoided when possible,
-- they usually indicate a programming error.
pokeException :: T.Text -> Poke a
pokeException msg = Poke $ \_ off -> throwIO (PokeException off msg)

------------------------------------------------------------------------
-- Peek monad

-- | 'Peek' actions are useful for building sequential deserializers.
--
-- They are actions which read from memory and construct values from it.
-- The 'Applicative' and 'Monad' instances make it easy to chain these
-- together to get more complicated deserializers. This machinery keeps
-- track of the current 'Ptr' and end-of-buffer 'Ptr'.
newtype Peek a = Peek
    { runPeek :: forall byte. Ptr byte -> Ptr byte -> IO (Ptr byte, a)
      -- ^ Run the 'Peek' action, with a 'Ptr' to the end of the buffer
      -- where data is poked, and a 'Ptr' to the current position. The
      -- result is the 'Ptr', along with a return value.
      --
      -- May throw a 'PeekException' if the memory contains invalid
      -- values.
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

-- | Throws a 'PeekException'.
peekException :: T.Text -> Peek a
peekException msg = Peek $ \end ptr -> throwIO (PeekException (end `minusPtr` ptr) msg)
