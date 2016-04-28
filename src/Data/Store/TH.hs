{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Store.TH
    (
    -- * Testing Store instances
      smallcheckManyStore
    , checkRoundtrip
    ) where

import           Data.Complex ()
import           Data.Store.Impl
import           Data.Store.TH.Internal (deriveStore)
import           Debug.Trace (trace)
import           Language.Haskell.TH
import           TH.Derive (Deriver(..))
import           TH.ReifyDataType (reifyDataTypeSubstituted, dtCons)
import           TH.Utilities (expectTyCon1)
import           Test.Hspec
import           Test.Hspec.SmallCheck (property)
import           Test.SmallCheck

instance Deriver (Store a) where
    runDeriver _ preds ty = do
        argTy <- expectTyCon1 ''Store ty
        dt <- reifyDataTypeSubstituted argTy
        (:[]) <$> deriveStore preds argTy (dtCons dt)

------------------------------------------------------------------------
-- Testing

-- | Test a 'Store' instance using 'smallcheck' and 'hspec'.
smallcheckManyStore :: Bool -> Int -> [TypeQ] -> ExpQ
smallcheckManyStore verbose depth = smallcheckMany . map testRoundtrip
  where
    testRoundtrip tyq = do
        ty <- tyq
        expr <- [e| property $ changeDepth (\_ -> depth) $ \x -> checkRoundtrip verbose (x :: $(return ty)) |]
        return ("Roundtrips (" ++ pprint ty ++ ")", expr)

-- | Check if a given value succeeds in decoding its encoded
-- representation.
checkRoundtrip :: (Eq a, Show a, Store a) => Bool -> a -> Bool
checkRoundtrip verbose x = decoded == Right x
  where
    encoded = verboseTrace verbose "encoded" (encode x)
    decoded = verboseTrace verbose "decoded" (decode encoded)

smallcheckMany :: [Q (String, Exp)] -> ExpQ
smallcheckMany = doE . map (\f -> f >>= \(name, expr) -> noBindS [e| it name $ $(return expr) |])

verboseTrace :: Show a => Bool -> String -> a -> a
verboseTrace True msg x = trace (show (msg, x)) x
verboseTrace False _ x = x
