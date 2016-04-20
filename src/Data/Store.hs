module Data.Store
    (
    -- * Encoding and decoding strict ByteStrings.
      encode, decode, decodeWith, decodeEx, decodeExWith
    -- * Store class and related types.
    , Store(..), Size(..), Poke, Peek
    -- ** Exceptions thrown by Peek
    , PeekException(..), peekException
    -- ** Statically sized data
    , StaticSize, IsStaticSize(..)
    ) where

import Data.Store.Internal
