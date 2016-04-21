{-# LANGUAGE LambdaCase #-}
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

-}
module Data.Store.Streaming
       ( -- * 'Message's to stream data using 'Store' for serialisation.
         Message (..)
         -- * Encoding 'Message's
       , encodeMessage
         -- * Decoding 'Message's
       , PeekMessage (..)
       , peekMessage
       , decodeMessage
       ) where

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.Store
import           Data.Store.Impl (Peek (..), Poke (..), tooManyBytes, getSize)
import           Data.Word
import           Foreign.Ptr
import qualified Foreign.Storable as Storable
import           System.IO.ByteBuffer

-- | If @a@ is an instance of 'Store', @Message a@ can be serialised
-- and deserialised in a streaming fashion.
newtype Message a = Message { fromMessage :: a } deriving Show

-- | Type used to store the length of a 'Message'.
type SizeTag = Int

tagLength :: Int
tagLength = Storable.sizeOf (undefined :: SizeTag)

-- | Encode a 'Message' to a 'ByteString'.
encodeMessage :: Store a => Message a -> ByteString
encodeMessage (Message x) =
    let l = getSize size x
    in BS.unsafeCreate
       (Storable.sizeOf (undefined :: SizeTag) + l)
       (\p -> runPoke (poke l >> poke x) p 0 (\_ _ -> return ()))

-- | The result of peeking at the next message can either be a
-- successfully deserialised 'Message', or a request for more input.
data PeekMessage m a = Done (Message a)
                     | NeedMoreInput (ByteString -> m (PeekMessage m a))

-- | Decode a 'Message' of known size from a 'ByteBuffer'.
peekSized :: (MonadIO m, Store a) => ByteBuffer -> Int -> m (PeekMessage m a)
peekSized bb n =
    unsafeConsume bb n >>= \case
        Right ptr -> Done . Message <$> decodeFromPtr ptr n
        Left _ -> return $ NeedMoreInput (\ bs -> copyByteString bb bs >> peekSized bb n)

-- | Decode a 'SizeTag' from a 'ByteBuffer'.
peekSizeTag :: MonadIO m => ByteBuffer -> m (PeekMessage m SizeTag)
peekSizeTag bb = peekSized bb tagLength

-- | Decode some 'Message' from a 'ByteBuffer', by first reading its
-- size, and then the actual 'Message'.
peekMessage :: (MonadIO m, Store a) => ByteBuffer -> m (PeekMessage m a)
peekMessage bb =
   peekSizeTag bb >>= \case
        (Done (Message n)) -> peekSized bb n
        NeedMoreInput _ ->
            return $ NeedMoreInput (\ bs -> copyByteString bb bs >> peekMessage bb)

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
    decodeSizeTag bb getBs >>= \case
        Nothing -> return Nothing
        Just n -> decodeSized bb getBs n

decodeSizeTag :: MonadIO m
              => ByteBuffer
              -> m (Maybe ByteString)
              -> m (Maybe SizeTag)
decodeSizeTag bb getBs =
    peekSizeTag bb >>= \case
        (Done (Message n)) -> return (Just n)
        (NeedMoreInput _) -> getBs >>= \case
            Just bs -> copyByteString bb bs >> decodeSizeTag bb getBs
            Nothing -> availableBytes bb >>= \case
                0 -> return Nothing
                n -> liftIO $ tooManyBytes tagLength n "Data.Store.Message.SizeTag"

decodeSized :: (MonadIO m, Store a)
            => ByteBuffer
            -> m (Maybe ByteString)
            -> Int
            -> m (Maybe (Message a))
decodeSized bb getBs n =
    peekSized bb n >>= \case
        Done message -> return (Just message)
        NeedMoreInput _ -> getBs >>= \case
            Just bs -> copyByteString bb bs >> decodeSized bb getBs n
            Nothing -> availableBytes bb >>= \ available ->
                liftIO $ tooManyBytes n available "Data.Store.Message.Message"

-- | Decode a value, given a 'Ptr' and the number of bytes that make
-- up the encoded message.
decodeFromPtr :: (MonadIO m, Store a) => Ptr Word8 -> Int -> m a
decodeFromPtr ptr n =
    liftIO $ snd <$> runPeek peek (ptr `plusPtr` n) ptr
