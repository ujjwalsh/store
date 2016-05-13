{-# LANGUAGE TemplateHaskell #-}

-- | This module exports TH utilities intended to be useful to users.
--
-- However, the visible exports do not show the main things that will be
-- useful, which is using TH to generate 'Store' instances, via
-- "TH.Derive".  It's used like this:
--
-- @
--     data Foo = Foo Int | Bar Int
--
--     $($(derive [d|
--         instance Deriving (Store Foo)
--         |]))
-- @
module Data.Store.TH
    (
    -- * Testing Store instances
      smallcheckManyStore
    , checkRoundtrip
    , assertRoundtrip
    ) where

import Data.Complex ()
import Data.Store.Impl
import Data.Typeable (Typeable, typeOf)
import Debug.Trace (trace)
import Language.Haskell.TH
import Prelude
import Test.Hspec
import Test.Hspec.SmallCheck (property)
import Test.SmallCheck

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

assertRoundtrip :: (Eq a, Show a, Store a, Monad m, Typeable a) => Bool -> a -> m ()
assertRoundtrip verbose x
    | checkRoundtrip verbose x = return ()
    | otherwise = fail $ "Failed to roundtrip "  ++ show (typeOf x)

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
