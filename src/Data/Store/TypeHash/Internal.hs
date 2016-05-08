{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Store.TypeHash.Internal where

import           Control.DeepSeq (NFData)
import           Control.Monad (when)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as BS
import           Data.Data (Data)
import           Data.Generics (listify)
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Proxy (Proxy(..))
import           Data.Store
import           Data.Store.Internal
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Instances.TH.Lift ()
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany (reifyMany)
import           Language.Haskell.TH.Syntax (Lift(lift))

newtype TaggedTH a = TaggedTH { unTaggedTH :: a }
    deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance NFData a => NFData (TaggedTH a)

instance (Store a, HasTypeHash a) => Store (TaggedTH a) where
    size = addSize 20 (contramapSize unTaggedTH size)
    peek = do
        tag <- peek
        let expected = typeHash (Proxy :: Proxy a)
        when (tag /= expected) $ fail "Mismatched type hash"
        peek
    poke x = do
        poke (typeHash (Proxy :: Proxy a))
        poke x

newtype TypeHash = TypeHash { unTypeHash :: StaticSize 20 BS.ByteString }
    deriving (Eq, Ord, Show, Store, Data, Typeable, Generic)

instance NFData TypeHash

instance Lift TypeHash where
    lift = liftStaticSize [t| BS.ByteString |] . unTypeHash

-- TODO: move into th-reify-many
reifyManyTyDecls :: ((Name, Info) -> Q (Bool, [Name]))
                 -> [Name]
                 -> Q [(Name, Info)]
reifyManyTyDecls f = reifyMany go
  where
    go x@(_, TyConI{}) = f x
    go x@(_, FamilyI{}) = f x
    go x@(_, PrimTyConI{}) = f x
    go x@(_, DataConI{}) = f x
    go (_, ClassI{}) = return (False, [])
    go (_, ClassOpI{}) = return (False, [])
    go (_, VarI{}) = return (False, [])
    go (_, TyVarI{}) = return (False, [])

-- | At compiletime, this yields a hash of the specified datatypes.
-- Not only does this cover the datatypes themselves, but also all
-- transitive dependencies.
--
-- The resulting expression is a literal of type 'Int'.
typeHashForNames :: [Name] -> Q Exp
typeHashForNames ns = do
    infos <- getTypeInfosRecursively ns
    [| TypeHash (BS.pack $(lift (BS.unpack (SHA1.hash (encode infos))))) |]

-- | At compiletime, this yields a cryptographic hash of the specified 'Type',
-- including the definition of things it references (transitively).
--
-- The resulting expression is a literal of type 'Int'.
--
-- Note: this funtion will succeed silently even if the names present in the
-- 'Type' cannot be found, which can happen if for example you do @hashOfType [t|Foo|]@
-- with @Foo@ is defined in the same module. Thus, you should never use it,
-- and instead use functions that output declarations such as 'mkHasTypeHash' and
-- 'mkManyHasTypeHash'.
hashOfType :: Type -> Q Exp
hashOfType ty = do
    infos <- getTypeInfosRecursively (listify (\_ -> True) ty)
    lift $ TypeHash $ toStaticSizeEx $ SHA1.hash $ encode (ty, infos)

getTypeInfosRecursively :: [Name] -> Q [(Name, Info)]
getTypeInfosRecursively names = do
    allInfos <- reifyManyTyDecls (\(_, info) -> return (True, listify (\_ -> True) info)) names
    -- Sorting step probably unnecessary because this should be
    -- deterministic, but hey why not.
    return (sortBy (comparing fst) allInfos)

-- TODO: Generic instance for polymorphic types, or have TH generate
-- polymorphic instances.

class HasTypeHash a where
    typeHash :: Proxy a -> TypeHash

mkHasTypeHash :: Type -> Q [Dec]
mkHasTypeHash ty =
    [d| instance HasTypeHash $(return ty) where
            typeHash _ = TypeHash $(hashOfType ty)
      |]

mkManyHasTypeHash :: [Q Type] -> Q [Dec]
mkManyHasTypeHash qtys = concat <$> mapM (mkHasTypeHash =<<) qtys

combineTypeHashes :: [TypeHash] -> TypeHash
combineTypeHashes = TypeHash . toStaticSizeEx . SHA1.hash . BS.concat . map (unStaticSize . unTypeHash)
