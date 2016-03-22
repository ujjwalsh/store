{-# LANGUAGE TemplateHaskell #-}

module Data.Store.TH where

import           Control.Applicative
import           Data.Complex ()
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Traversable (forM)
import           Foreign.C.Types ()
import           Foreign.Ptr (IntPtr, WordPtr, Ptr, FunPtr)
import           Foreign.StablePtr (StablePtr)
import           Foreign.Storable (Storable)
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany.Internal (TypeclassInstance(..), getInstances, unAppsT)
import           Language.Haskell.TH.Syntax (mkNameG_tc, mkNameG_v)
import           System.Posix.Types ()

-- TODO: This pattern of "define this set of instances given some other
-- instances / values" could be libraryized similarly to
-- mgsloan/instance-templates.

-- TODO: Generate inline pragmas? Probably not necessary

deriveManyStoreFromStorable =
    deriveManyStoreFromStorableImpl . (\pred x -> pred x && not (isPtrType x))

isPtrType :: Type -> Bool
isPtrType (ConT n) = n `elem` [''StablePtr, ''IntPtr, ''WordPtr, ''Ptr, ''FunPtr]
isPtrType _ = False

deriveManyStoreFromStorableImpl :: (Type -> Bool) -> Q [Dec]
deriveManyStoreFromStorableImpl pred = do
    -- Filters out overlapping instances and instances with more than
    -- one type arg (should be impossible).
    let postprocess = M.mapMaybeWithKey $ \tys xs ->
            case (tys, xs) of
                ([ty], [x]) -> Just x
                _ -> Nothing
    storables <- postprocess . instancesMap <$> getInstances ''Storable
    stores <- postprocess . instancesMap <$> getInstances storeName
    return $ M.elems $ flip M.mapMaybe (storables `M.difference` stores) $ \(TypeclassInstance cxt ty _) ->
        let argTy = head (tail (unAppsT ty)) in
        if pred argTy
            then Just $
                InstanceD cxt
                          (AppT (ConT storeName) argTy)
                          [ ValD (VarP (mkName "size")) (NormalB (VarE sizeStorableName)) []
                          , ValD (VarP (mkName "peek")) (NormalB (VarE peekStorableName)) []
                          , ValD (VarP (mkName "poke")) (NormalB (VarE pokeStorableName)) []
                          ]
            else Nothing

-- TOOD: move these to th-reify-many

-- | Get a map from the 'getTyHead' type of instances to
-- 'TypeclassInstance'.
instancesMap :: [TypeclassInstance] -> M.Map [Type] [TypeclassInstance]
instancesMap =
    M.fromListWith (++) .
    map (\ti -> (map getTyHead (instanceArgTypes ti), [ti]))

instanceArgTypes :: TypeclassInstance -> [Type]
instanceArgTypes (TypeclassInstance _ ty _) = drop 1 (unAppsT ty)

getTyHead :: Type -> Type
getTyHead (SigT x _) = getTyHead x
getTyHead (ForallT _ _ x) = getTyHead x
getTyHead (AppT l _) = getTyHead l
getTyHead x = x

-- Name hacks necessary due to https://ghc.haskell.org/trac/ghc/ticket/1012

-- Nice trick from the singletons package
storePkg :: String
storePkg = $( (LitE . StringL . loc_package) `fmap` location )

storeName, sizeStorableName, peekStorableName, pokeStorableName :: Name
storeName = mkNameG_tc storePkg "Data.Store" "Store"
sizeStorableName = mkNameG_v storePkg "Data.Store" "sizeStorable"
peekStorableName = mkNameG_v storePkg "Data.Store" "peekStorable"
pokeStorableName = mkNameG_v storePkg "Data.Store" "pokeStorable"
