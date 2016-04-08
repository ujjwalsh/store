{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Complex (Complex(..))
import           Data.Containers (mapFromList, setFromList)
import           Data.Int
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.Monoid
import           Data.Sequence (Seq)
import           Data.Sequences (fromList)
import           Data.Set (Set)
import           Data.Store
import           Data.Store.TH
import           Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable (Storable)
import           GHC.Fingerprint.Type (Fingerprint(..))
import           GHC.Generics
import           GHC.Real (Ratio(..))
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Spec.TH
import           System.Posix.Types
import           Test.Hspec hiding (runIO)
import           Test.SmallCheck.Series

------------------------------------------------------------------------
-- Instances for base types

-- TODO: should be possible to do something clever where it only defines
-- instances that don't already exist.  For now, just doing it manually.

addMinAndMaxBounds :: forall a. (Bounded a, Eq a, Num a) => [a] -> [a]
addMinAndMaxBounds xs =
    (if (minBound :: a) `notElem` xs then [minBound] else []) ++
    (if (maxBound :: a) `notElem` xs && (maxBound :: a) /= minBound then maxBound : xs else xs)

-- Serial instances for (Num a, Bounded a) types. Only really
-- appropriate for the use here.

$(do let ns = [ ''CWchar, ''CUid, ''CUShort, ''CULong, ''CULLong, ''CIntMax
              , ''CUIntMax, ''CPtrdiff, ''CSChar, ''CShort, ''CUInt, ''CLLong
              , ''CLong, ''CInt, ''CChar, ''CTcflag, ''CSsize, ''CRLim, ''CPid
              , ''COff, ''CNlink, ''CMode, ''CIno, ''CGid, ''CDev
              , ''Word8, ''Word16, ''Word32, ''Word64, ''Word
              , ''Int8, ''Int16, ''Int32, ''Int64
              ]
         f n = [d| instance Monad m => Serial m $(conT n) where
                      series = generate (\_ -> addMinAndMaxBounds [0, 1]) |]
     concat <$> mapM f ns)

-- Serial instances for (Num a) types. Only really appropriate for the
-- use here.

$(do let ns = [ ''CUSeconds, ''CClock, ''CTime, ''CUChar, ''CSize, ''CSigAtomic
              ,  ''CSUSeconds, ''CFloat, ''CDouble, ''CSpeed, ''CCc
              ]
         f n = [d| instance Monad m => Serial m $(conT n) where
                      series = generate (\_ -> [0, 1]) |]
     concat <$> mapM f ns)

-- Serial instances for (Generic a) types.

$(do let ns = [ ''Any, ''All ]
         f n = [d| instance Monad m => Serial m $(conT n) |]
     concat <$> mapM f ns)

$(do let ns = [ ''Dual, ''Sum, ''Product, ''First, ''Last ]
         f n = [d| instance (Monad m, Serial m a) => Serial m ($(conT n) a) |]
     concat <$> mapM f ns)

instance Monad m => Serial m Fingerprint where
    series = generate (\_ -> [Fingerprint 0 0, Fingerprint maxBound maxBound])

instance Monad m => Serial m BS.ByteString where
    series = fmap BS.pack series

instance Monad m => Serial m LBS.ByteString where
    series = fmap LBS.pack series

instance (Monad m, Serial m a, Storable a) => Serial m (SV.Vector a) where
    series = fmap SV.fromList series

instance (Monad m, Serial m a) => Serial m (V.Vector a) where
    series = fmap V.fromList series

instance (Monad m, Serial m k, Serial m a, Ord k) => Serial m (Map k a) where
    series = fmap mapFromList series

instance (Monad m, Serial m a, Ord a) => Serial m (Set a) where
    series = fmap setFromList series

instance (Monad m, Serial m a) => Serial m (IntMap a) where
    series = fmap mapFromList series

instance Monad m => Serial m IntSet where
    series = fmap setFromList series

instance Monad m => Serial m Text where
    series = fmap fromList series

instance (Monad m, Serial m a) => Serial m (Seq a) where
    series = fmap fromList series

instance (Monad m, Serial m a) => Serial m (Complex a) where
    series = uncurry (:+) <$> (series >< series)

------------------------------------------------------------------------
-- Test datatypes for generics support

data Test
    = TestA Int64 Word32
    | TestB Bool
    | TestC
    | TestD BS.ByteString
    deriving (Eq, Show, Generic)
instance Store Test
instance Monad m => Serial m Test

data X = X
    deriving (Eq, Show, Generic)
instance Monad m => Serial m X
instance Store X

main :: IO ()
main = hspec $ do
    describe "Store on all monomorphic instances"
        $(do insts <- getAllInstanceTypes1 ''Store
             smallcheckManyStore verbose 3 . map return . filter isMonoType $ insts)
    describe "Store on all custom generic instances"
        $(smallcheckManyStore verbose 3
            [ [t| Test |]
            , [t| X |]
            ])
    describe "Manually listed polymorphic store instances"
        $(smallcheckManyStore verbose 3
            [ [t| SV.Vector Int8 |]
            , [t| V.Vector  Int8 |]
            , [t| Ratio     Int8 |]
            , [t| Complex   Int8 |]
            , [t| Dual      Int8 |]
            , [t| Sum       Int8 |]
            , [t| Product   Int8 |]
            , [t| First     Int8 |]
            , [t| Last      Int8 |]
            , [t| Maybe     Int8 |]
            , [t| Either    Int8 Int8 |]
            , [t| SV.Vector Int64 |]
            , [t| V.Vector  Int64 |]
            , [t| Ratio     Int64 |]
            , [t| Complex   Int64 |]
            , [t| Dual      Int64 |]
            , [t| Sum       Int64 |]
            , [t| Product   Int64 |]
            , [t| First     Int64 |]
            , [t| Last      Int64 |]
            , [t| Maybe     Int64 |]
            , [t| Either    Int64 Int64 |]
            , [t| (Int8, Int16) |]
            , [t| (Int8, Int16, Bool) |]
            , [t| (Bool, (), (), ()) |]
            , [t| (Bool, (), Int8, ()) |]
            -- Container-ey types
            , [t| [Int8] |]
            , [t| [Int64] |]
            , [t| Seq Int8 |]
            , [t| Seq Int64 |]
            , [t| Set Int8 |]
            , [t| Set Int64 |]
            , [t| IntMap Int8 |]
            , [t| IntMap Int64 |]
            , [t| Map Int8 Int8 |]
            , [t| Map Int64 Int64 |]
            ])
    it "Size of generic instance for single fieldless constructor is 0" $ do
        case size :: Size X of
            ConstSize 0 -> (return () :: IO ())
            _ -> fail "Empty datatype takes up space"
    it "Printing out polymorphic store instances" $ do
        putStrLn ""
        putStrLn "Not really a test - printing out known polymorphic store instances (which should all be tested above)"
        putStrLn ""
        mapM_ putStrLn
              $(do insts <- getAllInstanceTypes1 ''Store
                   lift $ map pprint $ filter (not . isMonoType) insts)
