module Data.Store.StreamingSpec where

import Data.Store.Streaming
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.Hspec
import qualified Data.Conduit.List as C
import Data.Conduit ((=$=), ($$))
import qualified Data.ByteString as BS
import Data.List (unfoldr)
import Data.Monoid
import qualified System.IO.ByteBuffer as BB

spec :: Spec
spec = do
  describe "conduitEncode and conduitDecode" $ do
    it "Roundtrips ([Int])." $ property roundtrip
    it "Roundtrips ([Int]), with chunked transfer." $ property roundtripChunked
  describe "peekMessage" $ do
    it "demands more input when needed." $ property askMore
    it "successfully decodes valid input." $ property peek

roundtrip :: [Int] -> Property IO
roundtrip xs = monadic $ do
  xs' <- C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    =$= conduitDecode 1
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs

roundtripChunked :: [Int] -> Property IO
roundtripChunked xs = monadic $ do
  bs <- C.sourceList xs
    =$= C.map Message
    =$= conduitEncode
    $$ C.fold (<>) mempty
  let chunks =
        unfoldr (\x -> case BS.splitAt 3 x of
                    (chunk, _) | BS.null chunk -> Nothing
                    (chunk, rest) -> Just (chunk, rest))
                bs
  xs' <- C.sourceList chunks
    =$= conduitDecode 10
    =$= C.map fromMessage
    $$ C.consume
  return $ xs' == xs

askMore :: Integer -> Property IO
askMore x = monadic $ do
  bb <- BB.new 10
  let bs = encodeMessage (Message x)
      bs' = snd . BS.splitAt (BS.length bs -2) $ bs
  BB.copyByteString bb bs'
  peekResult <- peekMessage bb :: IO (PeekMessage IO Integer)
  case peekResult of
    NeedMoreInput _ -> return True
    _ -> return False

peek :: Integer -> Property IO
peek x = monadic $ do
  bb <- BB.new 10
  let bs = encodeMessage (Message x)
  BB.copyByteString bb bs
  peekResult <- peekMessage bb :: IO (PeekMessage IO Integer)
  case peekResult of
    NeedMoreInput _ -> return False
    Done (Message x') -> return $ x' == x
