{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests for untrusted data.

module Data.Store.UntrustedSpec where

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Int
import           Data.Monoid
import           Data.Store
import           Data.String
import           Data.Text (Text)
import qualified Data.Vector as V
import           Test.Hspec

-- | Test suite.
spec :: Spec
spec =
    describe
        "Untrusted input throws error"
        (do describe
                "Array-like length prefixes"
                (do let sample
                            :: IsString s
                            => s
                        sample = "abc"
                        list :: [Int]
                        list = [1, 2, 3]
                    it
                        "ByteString"
                        (shouldBeRightWrong 1234 (sample :: ByteString))
                    it
                        "Lazy ByteString"
                        (shouldBeRightWrong 1234 (sample :: L.ByteString))
                    it "Text" (shouldBeRightWrong 1234 (sample :: Text))
                    it "String" (shouldBeRightWrong 1234 (sample :: String))
                    it "Vector Int" (shouldBeRightWrong 1234 (V.fromList list))
                    it
                        "Vector Char"
                        (shouldBeRightWrong 1234 (V.fromList sample)))
            describe
                "Constructor tags"
                (do it
                        "Invalid constructor tag"
                        (shouldBe
                             (first
                                  (const ())
                                  (decode "\2" :: Either PeekException (Maybe ())))
                             (Left ()))
                    it
                        "Missing slots"
                        (shouldBe
                             (first
                                  (const ())
                                  (decode "\1" :: Either PeekException (Maybe Char)))
                             (Left ()))))

-- | Check decode.encode==id and then check decode.badencode=>error.
shouldBeRightWrong
    :: forall i.
       (Store i, Eq i, Show i)
    => Int64 -> i -> IO ()
shouldBeRightWrong len input = do
    shouldBe (decode (encode input) :: Either PeekException i) (Right input)
    shouldBe
        (first
             (const ())
             (decode (encodeWrongPrefix len input) :: Either PeekException i))
        (Left ())

-- | Encode a thing with the wrong length prefix.
encodeWrongPrefix :: Store thing => Int64 -> thing -> ByteString
encodeWrongPrefix len thing = encode len <> encodeThingNoPrefix thing

-- | Encode the thing and drop the length prefix.
encodeThingNoPrefix :: Store thing => thing -> ByteString
encodeThingNoPrefix = S.drop (S.length (encode (1 :: Int64))) . encode
