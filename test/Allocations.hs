{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Weigh Store's operations.

module Main where

import           Control.DeepSeq
import           Data.List
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import qualified Data.Vector as Boxed
import qualified Data.Vector.Serialize ()
import qualified Data.Vector.Storable as Storable
import           Text.Printf
import           Weigh

-- | Main entry point.
main :: IO ()
main =
  mainWith encoding

-- | Weigh encoding with Store vs Cereal.
encoding :: Weigh ()
encoding =
  do fortype "[Int]" (\n -> replicate n 0 :: [Int])
     fortype "Boxed Vector Int" (\n -> Boxed.replicate n 0 :: Boxed.Vector Int)
     fortype "Storable Vector Int"
             (\n -> Storable.replicate n 0 :: Storable.Vector Int)
  where fortype label make =
          scale (\(n,nstr) ->
                   do let title :: String -> String
                          title for = printf "%12s %-20s %s" nstr (label :: String) for
                      action (title "Allocate")
                             (return (make n))
                      action (title "Encode: Store")
                             (return (Store.encode (force (make n))))
                      action (title "Encode: Cereal")
                             (return (Cereal.encode (force (make n)))))
        scale func =
          mapM_ func
                (map (\x -> (x,commas x))
                     [1000000,2000000,10000000])
