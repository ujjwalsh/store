{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.DeepSeq
import           Criterion.Main
import qualified Data.ByteString as BS
import           Data.Int
import           Data.Store
import           Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import           Data.Word
import           GHC.Generics

-- TODO: add packer
#if COMPARISON_BENCH
import qualified Data.Binary as Binary
import qualified Data.Serialize as Cereal
import qualified Data.Binary.Serialise.CBOR as CBOR
#endif

main :: IO ()
main = do
    defaultMain
        [ bgroup "encode"
            [ benchEncode (0 :: Int)
#if !COMPARISON_BENCH
            , benchEncode' "1kb storable" (SV.fromList ([1..256] :: [Int32]))
            , benchEncode' "10kb storable" (SV.fromList ([1..(256 * 10)] :: [Int32]))
            , benchEncode' "1kb normal" (V.fromList ([1..256] :: [Int32]))
            , benchEncode' "10kb normal" (V.fromList ([1..(256 * 10 * 1024)] :: [Int32]))
#endif
            , benchEncode (SmallProduct 0 1 2 3)
            , benchEncode (SmallProductManual 0 1 2 3)
            , benchEncode [SS1 1, SS2 2, SS3 3, SS4 4]
            , benchEncode [SSM1 1, SSM2 2, SSM3 3, SSM4 4]
            ]
        , bgroup "decode"
            [ benchDecode (0 :: Int)
#if !COMPARISON_BENCH
            , benchDecode' "1kb storable" (SV.fromList ([1..256] :: [Int32]))
            , benchDecode' "10kb storable" (SV.fromList ([1..(256 * 10)] :: [Int32]))
            , benchDecode' "1kb normal" (V.fromList ([1..256] :: [Int32]))
            , benchDecode' "10kb normal" (V.fromList ([1..(256 * 10 * 1024)] :: [Int32]))
#endif
            , benchDecode (SmallProduct 0 1 2 3)
            , benchDecode (SmallProductManual 0 1 2 3)
            , benchDecode [SS1 1, SS2 2, SS3 3, SS4 4]
            , benchDecode [SSM1 1, SSM2 2, SSM3 3, SSM4 4]
            ]
        ]

type Ctx a =
    ( Store a, Typeable a, NFData a
#if COMPARISON_BENCH
    , Binary.Binary a
    , Cereal.Serialize a
#endif
    )

benchEncode :: Ctx a => a -> Benchmark
benchEncode = benchEncode' ""

benchEncode' :: Ctx a => String -> a -> Benchmark
benchEncode' msg x0 =
    env (return x0) $ \x ->
        let label = msg ++ " (" ++ show (typeOf x0) ++ ")"
            benchStore name = bench name (nf encode x) in
#if COMPARISON_BENCH
        bgroup label
            [ benchStore "store"
            , bench "binary" (nf Binary.encode x)
            , bench "cereal" (nf Cereal.encode x)
            ]
#else
        benchStore label
#endif

benchDecode :: Ctx a => a -> Benchmark
benchDecode = benchEncode' ""

-- TODO: comparison bench for decode

benchDecode' :: forall a. Ctx a => String -> a -> Benchmark
benchDecode' prefix x0 =
    env (return (encode x0)) $ \x ->
        bench (prefix ++ " (" ++ show (typeOf x0) ++ ")") (nf (decodeEx :: BS.ByteString -> a) x)

------------------------------------------------------------------------
-- Serialized datatypes

data SmallProduct = SmallProduct Int32 Int32 Int32 Int32
    deriving (Generic, Show, Typeable)

instance NFData SmallProduct
instance Store SmallProduct

data SmallProductManual = SmallProductManual Int32 Int32 Int32 Int32
    deriving (Generic, Show, Typeable)

instance NFData SmallProductManual
instance Store SmallProductManual where
    size = ConstSize 16
    peek = SmallProductManual <$> peek <*> peek <*> peek <*> peek
    poke (SmallProductManual a b c d) = poke a *> poke b *> poke c *> poke d

data SmallSum
    = SS1 Int8
    | SS2 Int32
    | SS3 Int64
    | SS4 Word32
    deriving (Generic, Show, Typeable)

instance NFData SmallSum
instance Store SmallSum

data SmallSumManual
    = SSM1 Int8
    | SSM2 Int32
    | SSM3 Int64
    | SSM4 Word32
    deriving (Generic, Show, Typeable)

instance NFData SmallSumManual
instance Store SmallSumManual where
    size = VarSize $ \x -> 1 + case x of
        SSM1{} -> 1
        SSM2{} -> 4
        SSM3{} -> 8
        SSM4{} -> 4
    peek = do
        tag <- peek
        case tag :: Word8 of
            0 -> SSM1 <$> peek
            1 -> SSM2 <$> peek
            2 -> SSM3 <$> peek
            3 -> SSM4 <$> peek
            _ -> fail "Invalid tag"
    poke (SSM1 x) = poke (0 :: Word8) >> poke x
    poke (SSM2 x) = poke (1 :: Word8) >> poke x
    poke (SSM3 x) = poke (2 :: Word8) >> poke x
    poke (SSM4 x) = poke (3 :: Word8) >> poke x

-- TODO: add TH generation of the above, and add LargeSum / LargeProduct cases

#if COMPARISON_BENCH
instance Binary.Binary SmallProduct
instance Binary.Binary SmallSum
instance Cereal.Serialize SmallProduct
instance Cereal.Serialize SmallSum

instance Binary.Binary SmallProductManual where
    get = SmallProductManual <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
    put (SmallProductManual a b c d) = Binary.put a *> Binary.put b *> Binary.put c *> Binary.put d

instance Binary.Binary SmallSumManual where
    get = do
        tag <- Binary.get
        case tag :: Word8 of
            0 -> SSM1 <$> Binary.get
            1 -> SSM2 <$> Binary.get
            2 -> SSM3 <$> Binary.get
            3 -> SSM4 <$> Binary.get
            _ -> fail "Invalid tag"
    put (SSM1 x) = Binary.put (0 :: Word8) *> Binary.put x
    put (SSM2 x) = Binary.put (1 :: Word8) *> Binary.put x
    put (SSM3 x) = Binary.put (2 :: Word8) *> Binary.put x
    put (SSM4 x) = Binary.put (3 :: Word8) *> Binary.put x

instance Cereal.Serialize SmallProductManual where
    get = SmallProductManual <$> Cereal.get <*> Cereal.get <*> Cereal.get <*> Cereal.get
    put (SmallProductManual a b c d) = Cereal.put a *> Cereal.put b *> Cereal.put c *> Cereal.put d

instance Cereal.Serialize SmallSumManual where
    get = do
        tag <- Cereal.get
        case tag :: Word8 of
            0 -> SSM1 <$> Cereal.get
            1 -> SSM2 <$> Cereal.get
            2 -> SSM3 <$> Cereal.get
            3 -> SSM4 <$> Cereal.get
            _ -> fail "Invalid tag"
    put (SSM1 x) = Cereal.put (0 :: Word8) *> Cereal.put x
    put (SSM2 x) = Cereal.put (1 :: Word8) *> Cereal.put x
    put (SSM3 x) = Cereal.put (2 :: Word8) *> Cereal.put x
    put (SSM4 x) = Cereal.put (3 :: Word8) *> Cereal.put x
#endif