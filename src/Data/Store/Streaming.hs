{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module: Data.Store.Streaming
Description: A thin streaming layer that uses 'Store' for serialisation.

For efficiency reasons, 'Store' does not provide facilities for
incrementally consuming input.  In order to avoid partial input, this
module introduces 'Message's that wrap values of instances of 'Store'.

In addition to the serialisation of a value, the serialised message
also contains the length of the serialisation.  This way, instead of
consuming input incrementally, more input can be demanded before
serialisation is attempted in the first place.

Each message starts with a fixed magic number, in order to detect
(randomly) invalid data.

-}
module Data.Store.Streaming
       ( -- * 'Message's to stream data using 'Store' for serialisation.
         Message (..)
       , headerLength
         -- * Encoding 'Message's
       , encodeMessage
         -- * Decoding 'Message's
       , PeekMessage (..)
       , peekMessage
       , decodeMessage
         -- * Conduits for encoding and decoding
       , conduitEncode
       , conduitDecode
       ) where

import           Control.Exception (throwIO)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Store
import           Data.Store.Impl (getSize)
import           Data.Store.Core (tooManyBytes, decodeIOWithFromPtr, unsafeEncodeWith)
import qualified Data.Text as T
import           Data.Word
import           Foreign.Ptr
import qualified Foreign.Storable as Storable
import           Prelude
import           System.IO.ByteBuffer (ByteBuffer)
import qualified System.IO.ByteBuffer as BB

-- | If @a@ is an instance of 'Store', @Message a@ can be serialised
-- and deserialised in a streaming fashion.
newtype Message a = Message { fromMessage :: a } deriving (Eq, Show)

-- | Type used to store the length of a 'Message'.
type SizeTag = Int

-- | Some fixed arbitrary magic number that precedes every 'Message'.
messageMagic :: Word64
messageMagic = 18205256374652458875

magicLength :: Int
magicLength = Storable.sizeOf messageMagic

sizeTagLength :: Int
sizeTagLength = Storable.sizeOf (undefined :: SizeTag)

-- | Number of bytes needed for the magic number and size tag.
headerLength :: Int
headerLength = magicLength + sizeTagLength

-- | Encode a 'Message' to a 'ByteString'.
encodeMessage :: Store a => Message a -> ByteString
encodeMessage (Message x) =
    unsafeEncodeWith pokeFunc totalLength
  where
    bodyLength = getSize x
    totalLength = headerLength + bodyLength
    pokeFunc = do
        poke messageMagic
        poke bodyLength
        poke x
{-# INLINE encodeMessage #-}

-- | The result of peeking at the next message can either be a
-- successfully deserialised 'Message', or a request for more input.
data PeekMessage m a = Done (Message a)
                     | NeedMoreInput (ByteString -> m (PeekMessage m a))

-- | Decode a 'Message' of known size from a 'ByteBuffer'.
peekSized :: (MonadIO m, Store a) => ByteBuffer -> Int -> m (PeekMessage m a)
peekSized bb n =
    BB.unsafeConsume bb n >>= \case
        Right ptr -> liftM (Done . Message) $ decodeFromPtr ptr n
        Left _ -> return $ NeedMoreInput (\ bs -> BB.copyByteString bb bs
                                                  >> peekSized bb n)
{-# INLINE peekSized #-}

-- | Decode a header (magic number and 'SizeTag') from a 'ByteBuffer'.
peekMessageHeader :: MonadIO m => ByteBuffer -> m (PeekMessage m SizeTag)
peekMessageHeader bb = do
    available <- BB.availableBytes bb
    if available < headerLength
        then return $ NeedMoreInput (\bs -> BB.copyByteString bb bs
                                            >> peekMessageHeader bb)
        else peekSized bb magicLength >>= \case
          Done (Message x) | x == messageMagic -> peekSized bb sizeTagLength
          Done (Message x) -> liftIO . throwIO $ PeekException 0 . T.pack $ "Wrong message magic, " ++ show x
          NeedMoreInput _ -> fail "Internal error in peekMessageHeader."
{-# INLINE peekMessageHeader #-}

-- | Decode some 'Message' from a 'ByteBuffer', by first reading its
-- header, and then the actual 'Message'.
peekMessage :: (MonadIO m, Store a) => ByteBuffer -> m (PeekMessage m a)
peekMessage bb =
   peekMessageHeader bb >>= \case
        (Done (Message n)) -> peekSized bb n
        NeedMoreInput _ ->
            return $ NeedMoreInput (\ bs -> BB.copyByteString bb bs
                                            >> peekMessage bb)
{-# INLINE peekMessage #-}

-- | Decode a 'Message' from a 'ByteBuffer' and an action that can get
-- additional 'ByteString's to refill the buffer when necessary.
--
-- The only conditions under which this function will give 'Nothing',
-- is when the 'ByteBuffer' contains zero bytes, and refilling yields
-- 'Nothing'.  If there is some data available, but not enough to
-- decode the whole 'Message', a 'PeekException' will be thrown.
decodeMessage :: (MonadIO m, Store a)
            => ByteBuffer -> m (Maybe ByteString) -> m (Maybe (Message a))
decodeMessage bb getBs =
    decodeHeader bb getBs >>= \case
        Nothing -> return Nothing
        Just n -> decodeSized bb getBs n
{-# INLINE decodeMessage #-}

decodeHeader :: MonadIO m
              => ByteBuffer
              -> m (Maybe ByteString)
              -> m (Maybe SizeTag)
decodeHeader bb getBs =
    peekMessageHeader bb >>= \case
        (Done (Message n)) -> return (Just n)
        (NeedMoreInput _) -> getBs >>= \case
            Just bs -> BB.copyByteString bb bs >> decodeHeader bb getBs
            Nothing -> BB.availableBytes bb >>= \case
                0 -> return Nothing
                n -> liftIO $ tooManyBytes headerLength n "Data.Store.Message.SizeTag"
{-# INLINE decodeHeader #-}

decodeSized :: (MonadIO m, Store a)
            => ByteBuffer
            -> m (Maybe ByteString)
            -> Int
            -> m (Maybe (Message a))
decodeSized bb getBs n =
    peekSized bb n >>= \case
        Done message -> return (Just message)
        NeedMoreInput _ -> getBs >>= \case
            Just bs -> BB.copyByteString bb bs >> decodeSized bb getBs n
            Nothing -> BB.availableBytes bb >>= \ available ->
                liftIO $ tooManyBytes n available "Data.Store.Message.Message"
{-# INLINE decodeSized #-}

-- | Decode a value, given a 'Ptr' and the number of bytes that make
-- up the encoded message.
decodeFromPtr :: (MonadIO m, Store a) => Ptr Word8 -> Int -> m a
decodeFromPtr ptr n = liftIO $ decodeIOWithFromPtr peek ptr n
{-# INLINE decodeFromPtr #-}

-- | Conduit for encoding 'Message's to 'ByteString's.
conduitEncode :: (Monad m, Store a) => C.Conduit (Message a) m ByteString
conduitEncode = C.map encodeMessage
{-# INLINE conduitEncode #-}

-- | Conduit for decoding 'Message's from 'ByteString's.
conduitDecode :: (MonadIO m, MonadResource m, Store a)
              => Maybe Int
              -- ^ Initial length of the 'ByteBuffer' used for
              -- buffering the incoming 'ByteString's.  If 'Nothing',
              -- use the default value of 4MB.
              -> C.Conduit ByteString m (Message a)
conduitDecode bufSize =
    C.bracketP
      (BB.new bufSize)
      BB.free
      go
  where
    go buffer = do
        mmessage <- decodeMessage buffer C.await
        case mmessage of
            Nothing -> return ()
            Just message -> C.yield message >> go buffer
{-# INLINE conduitDecode #-}
