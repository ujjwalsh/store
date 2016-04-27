{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Monad (unless)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Complex (Complex(..))
import           Data.Containers (mapFromList, setFromList)
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import           Data.Hashable (Hashable)
import           Data.Int
import           Data.IntMap (IntMap)
import           Data.IntSet (IntSet)
import           Data.Map (Map)
import           Data.Monoid
import           Data.Primitive.Types (Addr)
import           Data.Sequence (Seq)
import           Data.Sequences (fromList)
import           Data.Set (Set)
import           Data.Store
import           Data.Store.Internal
import           Data.Store.TH
import           Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV
import           Data.Void (Void)
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable (Storable)
import           GHC.Fingerprint.Type (Fingerprint(..))
import           GHC.Generics
import           GHC.Real (Ratio(..))
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany
import           Language.Haskell.TH.Syntax
import           Spec.TH
import qualified System.IO.ByteBufferSpec as BB
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

-- Serial instances for Primitive vectors

$(do tys <- getAllInstanceTypes1 ''PV.Prim
     let f ty = [d| instance (Serial m $(return ty), Monad m) => Serial m (PV.Vector $(return ty)) where
                      series = fmap PV.fromList series |]
     concat <$> mapM f tys)

-- Serial instances for (Generic a) types.

-- FIXME: generating for TH instances is probably just adding
-- unnecessary compiletime + runtime overhead.
$(do thNames <- reifyManyWithoutInstances ''Serial [''Info, ''Loc] (\_ -> True)
     let ns = [ ''Any, ''All ] ++ thNames
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

instance Monad m => Serial m SBS.ShortByteString where
    series = fmap SBS.pack series

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

instance (Monad m, Serial m a, UV.Unbox a) => Serial m (UV.Vector a) where
    series = fmap fromList series

instance (Monad m, Serial m k, Serial m a, Hashable k, Eq k) => Serial m (HashMap k a) where
    series = fmap mapFromList series

instance (Monad m, Serial m a, Hashable a, Eq a) => Serial m (HashSet a) where
    series = fmap setFromList series

instance Monad m => Serial m Time.Day where
    series = Time.ModifiedJulianDay <$> series

instance Monad m => Serial m Time.DiffTime where
    series = Time.picosecondsToDiffTime <$> series

instance Monad m => Serial m Time.UTCTime where
    series = uncurry Time.UTCTime <$> (series >< series)

-- Should probably get added to smallcheck :)
instance (Monad m) => Serial m Void where
    series = generate (\_ -> [])

deriving instance Show NameFlavour
deriving instance Show NameSpace

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
             addrTy <- [t| PV.Vector Addr |]
             let f ty = isMonoType ty && ty /= addrTy
             smallcheckManyStore verbose 2 . map return . filter f $ insts)
    describe "Store on all custom generic instances"
        $(smallcheckManyStore verbose 2
            [ [t| Test |]
            , [t| X |]
            ])
    describe "Manually listed polymorphic store instances"
        $(smallcheckManyStore verbose 2
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
            , [t| HashMap Int8 Int8 |]
            , [t| HashMap Int64 Int64 |]
            , [t| HashSet Int8 |]
            , [t| HashSet Int64 |]
            ])
    it "StaticSize roundtrips" $ do
        let x :: StaticSize 17 BS.ByteString
            x = toStaticSizeEx (BS.replicate 17 255)
        unless (checkRoundtrip False x) $
            (fail "Failed to roundtrip StaticSize ByteString" :: IO ())
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
    BB.spec
