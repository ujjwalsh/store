module Data.Store
    (
    -- * Encoding and decoding strict ByteStrings.
      encode, decode, decodeWith
    -- * Store class and related types.
    , Store(..), Size(..), Poke, Peek
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException
    ) where

import Data.Store.Internal
