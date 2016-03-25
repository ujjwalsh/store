{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.TH where

import Data.Generics.Schemes (listify)
import Data.Maybe (fromMaybe)
import Data.Store
import Language.Haskell.TH
import Language.Haskell.TH.ReifyMany.Internal (unAppsT, getInstances, TypeclassInstance(..))
import Safe (headMay)
import Test.Hspec
import Test.Hspec.SmallCheck (property)
import Test.SmallCheck

#if VERBOSE_TEST
import Debug.Trace (trace)
#endif

testMany :: [Q (String, Exp)] -> ExpQ
testMany = doE . map (\f -> f >>= \(name, expr) -> noBindS [e| it name $ $(return expr) |])

testManyRoundtrips :: Int -> [TypeQ] -> ExpQ
testManyRoundtrips depth = testMany . map testRoundtrip
  where
    testRoundtrip tyq = do
        ty <- tyq
        expr <- [e| property $ changeDepth (\_ -> depth) $ \x -> verboseTrace "decoded" (decode (verboseTrace "encoded" (encode x))) == Right (x :: $(return ty)) |]
        return ("Roundtrips (" ++ pprint ty ++ ")", expr)

verboseTrace :: Show a => String -> a -> a
#if VERBOSE_TEST
verboseTrace msg x = trace (show (msg, x)) x
#else
verboseTrace _ x = x
#endif

-- TODO: either generate random types that satisfy instances with
-- variables in them, or have a check that there's at least a manual
-- check for polymorphic instances.

getAllInstanceTypes :: Name -> Q [[Type]]
getAllInstanceTypes n =
    map (\(TypeclassInstance _ ty _) -> drop 1 (unAppsT ty)) <$>
    getInstances n

getAllInstanceTypes1 :: Name -> Q [Type]
getAllInstanceTypes1 n =
    fmap (fmap (fromMaybe (error "getAllMonoInstances1 expected only one type argument") . headMay))
         (getAllInstanceTypes n)

isMonoType :: Type -> Bool
isMonoType = null . listify isVarT
  where
    isVarT VarT{} = True
    isVarT _ = False
