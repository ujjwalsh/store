{-# LANGUAGE OverloadedStrings #-}
module System.IO.ByteBufferSpec where

import qualified Data.ByteString as BS
import qualified System.IO.ByteBuffer as BB
import           Test.Hspec

spec :: Spec
spec = describe "ByteBuffer" $ do
    it "can grow to store a value and return it." $ do
        bb <- BB.new 0
        let bs = "some bytestring"
        BB.copyByteString bb bs
        bs' <- BB.consume bb (BS.length bs)
        bs' `shouldBe` Right bs
        bbIsEmpty bb
    it "should request more input when needed." $ do
        bb <- BB.new 1
        let bs = "some bytestring"
        BB.copyByteString bb bs
        bs' <- BB.consume bb (2 * BS.length bs)
        bs' `shouldBe` Left (BS.length bs)
        BB.copyByteString bb bs
        bs'' <- BB.consume bb (2 * BS.length bs)
        bs'' `shouldBe` Right (BS.concat [bs, bs])
        bbIsEmpty bb
    it "should not grow if bytes can be freed." $ do
        let bs1 = "12345"
            bs2 = "67810" -- what about nine? 7 8 9!
        bb <- BB.new (BS.length bs1)
        BB.copyByteString bb bs1
        bs1' <- BB.consume bb (BS.length bs1)
        BB.copyByteString bb bs2
        bs2' <- BB.consume bb (BS.length bs2)
        bs1' `shouldBe` Right bs1
        bs2' `shouldBe` Right bs2
        bbSize <- BB.totalSize bb
        bbSize `shouldBe` BS.length bs1
        bbIsEmpty bb

bbIsEmpty :: BB.ByteBuffer -> Expectation
bbIsEmpty bb = BB.isEmpty bb >>= (`shouldBe` True)
