-- | This is the main public API of the store package. The functions
-- exported here are more likely to be stable between versions.
module Data.Store
    (
    -- * Encoding and decoding strict ByteStrings.
      encode,
      decode, decodeWith,
      decodeEx, decodeExWith, decodeExPortionWith,
      decodeIO, decodeIOWith, decodeIOPortionWith
    -- * Store class and related types.
    , Store(..), Size(..), Poke, Peek
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException
    ) where

import Data.Store.Internal
