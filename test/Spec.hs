{-# LANGUAGE DeriveGeneric #-}

import Control.Applicative
import Data.Store
import Data.Store.Generics
import Data.Store.Internal
import qualified GHC.Generics as G
import Control.Monad

data Test
    = TestA Int Double Float Int
    | TestB Bool
    deriving (Eq, Show, G.Generic)

instance Store Test where
    size = contramapSize G.from gsize
    poke = gpoke . G.from
    peek = G.to <$> gpeek

main :: IO ()
main = do
    let a = TestA 42 3.14 11 0
        b = TestB True
    when (decode (encode a) /= a) $ fail "FAIL"
    when (decode (encode b) /= b) $ fail "FAIL"
