{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module provides utilities which help ensure that we aren't
-- attempting to de-serialize data that is an older or newer version.
-- The 'WithVersion' utility wraps up a datatype along with a version
-- tag. This version tag can either be provided by the user
-- ('namedVersionConfig'), or use a computed hash
-- ('hashedVersionConfig').
--
-- The magic here is using an SYB traversal ('Data') to get the
-- structure of all the data-types involved. This info is rendered to
-- text and hashed to yield a hash which describes it.
module Data.Store.Version
    ( StoreVersion(..)
    , WithVersion(..)
    , VersionConfig(..)
    , hashedVersionConfig
    , namedVersionConfig
    , wrapVersion
    , checkVersion
    , VersionCheckException(..)
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString.Char8 as BS8
import           Data.Generics hiding (DataType, Generic)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Store.Internal
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as T
import           Data.Typeable.Internal (TypeRep(..))
import           GHC.Generics (Generic)
import           Language.Haskell.TH
import           System.Directory
import           System.Environment
import           System.FilePath
import           TH.RelativePaths
import           TH.Utilities

newtype StoreVersion = StoreVersion { unStoreVersion :: BS.ByteString }
    deriving (Eq, Show, Ord, Data, Typeable, Generic, Store)

data WithVersion a = WithVersion a StoreVersion
    deriving (Eq, Show, Ord, Data, Typeable, Generic)

instance Store a => Store (WithVersion a)

-- | Configuration for the version checking of a particular type.
data VersionConfig a = VersionConfig
    { vcExpectedHash :: Maybe String
      -- ^ When set, specifies the hash which is expected to be computed.
    , vcManualName :: Maybe String
      -- ^ When set, specifies the name to instead use to tag the data.
    , vcIgnore :: S.Set String
      -- ^ DataTypes to ignore.
    } deriving (Eq, Show, Data, Typeable, Generic)

hashedVersionConfig :: String -> VersionConfig a
hashedVersionConfig hash = VersionConfig
    { vcExpectedHash = Just hash
    , vcManualName = Nothing
    , vcIgnore = S.empty
    }

namedVersionConfig :: String -> String -> VersionConfig a
namedVersionConfig name hash = VersionConfig
    { vcExpectedHash = Just hash
    , vcManualName = Just name
    , vcIgnore = S.empty
    }

wrapVersion :: Data a => VersionConfig a -> Q Exp
wrapVersion = impl Wrap

checkVersion :: Data a => VersionConfig a -> Q Exp
checkVersion = impl Check

data WhichFunc = Wrap | Check

impl :: forall a. Data a => WhichFunc -> VersionConfig a -> Q Exp
impl wf vc = do
    let proxy = Proxy :: Proxy a
        info = encodeUtf8 (T.pack (getStructureInfo (vcIgnore vc) proxy))
        hash = SHA1.hash info
        hashb64 = BS8.unpack (B64Url.encode hash)
        version = case vcManualName vc of
            Nothing -> [e| StoreVersion hash |]
            Just name -> [e| StoreVersion name |]
    case vcExpectedHash vc of
        Nothing -> return ()
        Just expectedHash -> do
            let shownType = showsQualTypeRep 0 (typeRep proxy) ""
            -- FIXME: sanitize expected and handle null
            path <- storeVersionedPath expectedHash
            if hashb64 == expectedHash
                then writeVersionInfo path shownType info
                else do
                    newPath <- storeVersionedPath hashb64
                    writeVersionInfo newPath shownType info
                    exists <- runIO $ doesFileExist path
                    extraMsg <- if not exists
                        then return ", but no file found with previously stored structural info."
                        else return (", use something like the following to compare with the old structural info:\n\n" ++
                                     "diff -u " ++ show path ++ " " ++ show newPath)
                    error $
                        "For " ++ shownType ++ ",\n" ++
                        "Data.Store.Version expected hash " ++ show hashb64 ++
                        ", but " ++ show expectedHash ++ " is specified.\n" ++
                        "The data used to construct the hash has been written to " ++ show newPath ++
                        extraMsg ++ "\n"
    case wf of
        Wrap -> [e| (\x -> (x :: $(typeRepToType (typeRep proxy))) `WithVersion` $(version)) |]
        Check -> [e| (\(WithVersion x gotVersion) ->
                        if gotVersion /= $(version)
                            then Left (VersionCheckException
                                { expectedVersion = $(version)
                                , receivedVersion = gotVersion
                                })
                            else Right x) |]

{-
                            txtWithComments <- runIO $ T.readFile path
                            let txt = T.unlines $ dropWhile ("--" `T.isPrefixOf`) $ T.lines txtWithComments
                                storedHash = BS8.unpack (B64Url.encode (SHA1.hash (encodeUtf8 txt)))
                            if storedHash == expectedHash
                                then return (", compare with the structural info that matches the hash, found in " ++ show path)
                                else return (", but the old file found also doesn't match the hash.")
-}

writeVersionInfo :: FilePath -> String -> BS.ByteString -> Q ()
writeVersionInfo path shownType info = runIO $ do
    createDirectoryIfMissing True (takeDirectory path)
    T.writeFile path $ T.unlines $
        [ T.pack ("-- Structural info for type " ++ shownType)
        , "-- Generated by an invocation of functions in Data.Store.Version"
        ] ++ T.lines (decodeUtf8 info)

storeVersionedPath :: String -> Q FilePath
storeVersionedPath filename = do
    mstack <- runIO (lookupEnv "STACK_EXE")
    let dirName = case mstack of
            Just _ -> ".stack-work"
            Nothing -> "dist"
    pathRelativeToCabalPackage (dirName </> "store-versioned" </> filename)

-- Implementation details

data S = S
    { sResults :: M.Map String String
    , sCurResult :: String
    , sFieldNames :: [String]
    }

getStructureInfo :: forall a. Data a => S.Set String -> Proxy a -> String
getStructureInfo ignore = renderResults . sResults . flip execState (S M.empty "" []) . getStructureInfo' ignore
  where
    renderResults = unlines . map (\(k, v) -> k ++ v) . M.toAscList

getStructureInfo' :: forall a. Data a => S.Set String -> Proxy a -> State S ()
getStructureInfo' ignore _ = do
    s0 <- get
    when (M.notMember label (sResults s0)) $
        if S.member shownType ignore
            then setResult " ignored\n"
            else case dataTypeRep (dataTypeOf (undefined :: a)) of
                AlgRep cs -> do
                    setResult ""
                    mapM_ goConstr (zip (True : repeat False) cs)
                    result <- gets sCurResult
                    setResult (if null cs then result ++ "\n" else result)
                IntRep -> setResult " has IntRep\n"
                FloatRep -> setResult " has FloatRep\n"
                CharRep -> setResult " has CharRep\n"
                NoRep
                    | S.member shownType ignore -> setResult " has NoRep\n"
                    | otherwise -> error $
                        "\nNoRep in Data.Store.Version for " ++ show shownType ++
                        ".\nIn the future it will be possible to statically " ++
                        "declare a global serialization version for this type. " ++
                        "\nUntil then you will need to use 'vcIgnore', and " ++
                        "understand that serialization changes for affected types " ++
                        "will not be detected.\n"
  where
    setResult x =
         modify (\s -> S
             { sResults = M.insert label x (sResults s)
             , sCurResult = ""
             , sFieldNames = []
             })
    label = "data-type " ++ shownType
    shownType = showsQualTypeRep 0 (typeRep (Proxy :: Proxy a)) ""
    goConstr :: (Bool, Constr) -> State S ()
    goConstr (isFirst, c) = do
        modify (\s -> s
            { sFieldNames = constrFields c ++ map (\ix -> "slot " ++ show (ix :: Int)) [0..]
            , sCurResult = sCurResult s ++ (if isFirst then "\n  = " else "  | ") ++ showConstr c ++ " {\n"
            })
        void (fromConstrM goField c :: State S a)
        modify (\s -> s { sCurResult = sCurResult s ++ "  }\n" })
    goField :: forall b. Data b => State S b
    goField = do
        s <- get
        case sFieldNames s of
            [] -> fail "impossible case in getStructureInfo'"
            (name:names) -> do
                getStructureInfo' ignore (Proxy :: Proxy b)
                s' <- get
                put s
                    { sResults = sResults s'
                    , sCurResult = sCurResult s ++ "    " ++ name ++ " :: " ++ showsQualTypeRep 0 (typeRep (Proxy :: Proxy b)) "\n"
                    , sFieldNames = names
                    }
                return (error "unexpected evaluation")

showsQualTypeRep :: Int -> TypeRep -> ShowS
#if MIN_VERSION_base(4,8,0)
showsQualTypeRep p (TypeRep _ tycon _ tys) =
#else
showsQualTypeRep p (TypeRep _ tycon tys) =
#endif
    case tys of
        [] -> showsQualTyCon tycon
        [x] | tycon == tcList -> showChar '[' . showsQualTypeRep 0 x . showChar ']'
          where
        [a,r] | tycon == tcFun  -> showParen (p > 8) $
                                     showsQualTypeRep 9 a .
                                     showString " -> " .
                                     showsQualTypeRep 8 r
        xs | isTupleTyCon tycon -> showTuple xs
           | otherwise         ->
                showParen (p > 9) $
                showsQualTyCon tycon .
                showChar ' '      .
                showArgs (showChar ' ') tys

showsQualTyCon :: TyCon -> ShowS
showsQualTyCon tc = showString (tyConModule tc ++ "." ++ tyConName tc)

isTupleTyCon :: TyCon -> Bool
isTupleTyCon tc
  | ('(':',':_) <- tyConName tc = True
  | otherwise                   = False

showArgs :: ShowS -> [TypeRep] -> ShowS
showArgs _   []     = id
showArgs _   [a]    = showsQualTypeRep 10 a
showArgs sep (a:as) = showsQualTypeRep 10 a . sep . showArgs sep as

showTuple :: [TypeRep] -> ShowS
showTuple args = showChar '('
               . showArgs (showChar ',') args
               . showChar ')'

tcList :: TyCon
tcList = tyConOf (Proxy :: Proxy [()])

tcFun :: TyCon
tcFun = tyConOf (Proxy :: Proxy (Int -> Int))

tyConOf :: Typeable a => Proxy a -> TyCon
tyConOf = typeRepTyCon . typeRep

data VersionCheckException = VersionCheckException
    { expectedVersion :: StoreVersion
    , receivedVersion :: StoreVersion
    } deriving
#if MIN_VERSION_base(4,8,0)
        (Typeable, Show)

instance Exception VersionCheckException where
    displayException = displayVCE
#else
        (Typeable)

instance Show VersionCheckException where
    show = displayVCE

instance Exception VersionCheckException
#endif

displayVCE :: VersionCheckException -> String
displayVCE VersionCheckException{..} =
    "Mismatch detected by Data.Store.Version - expected " ++
    T.unpack (decodeUtf8With lenientDecode (unStoreVersion expectedVersion)) ++ " but got " ++
    T.unpack (decodeUtf8With lenientDecode (unStoreVersion receivedVersion))
