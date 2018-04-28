module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck


main :: IO ()
main = putStrLn "hello!"

-- spec :: Spec
-- spec =
--   describe "Lib" $ do
--     it "works" $ do
--       True `shouldBe` True
--     prop "ourAdd is commutative" $ \x y ->
--       ourAdd x y `shouldBe` ourAdd y x
