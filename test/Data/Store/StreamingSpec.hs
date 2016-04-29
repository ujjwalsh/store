{-# LANGUAGE LambdaCase #-}
module Data.Store.StreamingSpec where

import           Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import           Data.Conduit ((=$=), ($$))
import qualified Data.Conduit.List as C
import           Data.List (unfoldr)
import           Data.Monoid
import           Data.Store (PeekException (..))
import           Data.Store.Streaming
import qualified System.IO.ByteBuffer as BB
import           Test.Hspec
import           Test.Hspec.SmallCheck
import           Test.SmallCheck

spec :: Spec
spec = do
  describe "conduitEncode and conduitDecode" $ do
    it "Roundtrips ([Int])." $ property roundtrip
    it "Roundtrips ([Int]), with chunked transfer." $ property roundtripChunked
    it "Throws an Exception on incomplete messages." $ conduitIncomplete
  describe "peekMessage" $ do
    it "demands more input when needed." $ property (askMore 8)
    it "demands more input on incomplete SizeTag." $ property (askMore 1)
    it "successfully decodes valid input." $ property peek
  describe "decodeMessage" $
    it "Throws an Exception on incomplete messages." $ decodeIncomplete

roundtrip :: [Int] -> Property IO
roundtrip xs = monadic $ do
  xs' <- runResourceT $ C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    =$= conduitDecode Nothing
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs

roundtripChunked :: [Int] -> Property IO
roundtripChunked input = monadic $ do
  let (xs, chunkLengths) = splitAt (length input `div` 2) input
  bs <- C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    $$ C.fold (<>) mempty
  let chunks = unfoldr takeChunk (bs, chunkLengths)
  xs' <- runResourceT $ C.sourceList chunks
    =$= conduitDecode (Just 10)
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs
  where
    takeChunk (x, _) | BS.null x = Nothing
    takeChunk (x, []) = Just (x, (BS.empty, []))
    takeChunk (x, l:ls) =
        let (chunk, rest) = BS.splitAt l x
        in Just (chunk, (rest, ls))

conduitIncomplete :: Expectation
conduitIncomplete =
    (runResourceT (C.sourceList [incompleteInput]
                  =$= conduitDecode (Just 10)
                  $$ C.consume)
    :: IO [Message Integer]) `shouldThrow` peekException

-- splits an encoded message after n bytes.  Feeds the first part to
-- peekResult, expecting it to require more input.  Then, feeds the
-- second part and checks if the decoded result is the original
-- message.
askMore :: Int -> Integer -> Property IO
askMore n x = monadic $ BB.with (Just 10) $ \ bb -> do
  let bs = encodeMessage (Message x)
      (start, end) = BS.splitAt n $ bs
  BB.copyByteString bb start
  peekResult <- peekMessage bb :: IO (PeekMessage IO Integer)
  case peekResult of
    NeedMoreInput cont ->
      cont end >>= \case
        Done (Message x') -> return $ x' == x
        _ -> return False
    _ -> return False

peek :: Integer -> Property IO
peek x = monadic $ BB.with (Just 10) $ \ bb -> do
  let bs = encodeMessage (Message x)
  BB.copyByteString bb bs
  peekResult <- peekMessage bb :: IO (PeekMessage IO Integer)
  case peekResult of
    NeedMoreInput _ -> return False
    Done (Message x') -> return $ x' == x

decodeIncomplete :: IO ()
decodeIncomplete = BB.with (Just 0) $ \ bb -> do
  BB.copyByteString bb (BS.take 1 incompleteInput)
  (decodeMessage bb (return Nothing) :: IO (Maybe (Message Integer)))
    `shouldThrow` peekException

incompleteInput :: BS.ByteString
incompleteInput =
  let bs = encodeMessage (Message (42 :: Integer))
  in BS.take (BS.length bs - 1) bs

peekException :: Selector PeekException
peekException = const True
