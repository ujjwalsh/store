
module Data.Store.TypeHash
    ( TaggedTH(..)
    , TypeHash
    , HasTypeHash(..)
    -- * TH for generating HasTypeHash instances
    , mkHasTypeHash
    , mkManyHasTypeHash
    ) where

import Data.Store.TypeHash.Internal
