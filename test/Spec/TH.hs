{-# LANGUAGE CPP #-}

-- Just exists due to TH stage restriction... The actual testing TH code
-- is in "Data.Store.TH".
module Spec.TH where

verbose :: Bool
verbose =
#if VERBOSE_TEST
    True
#else
    False
#endif
