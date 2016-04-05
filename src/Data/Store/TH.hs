{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Store.TH where

import           Control.Applicative
import           Data.Complex ()
import qualified Data.Map as M
import           Data.Store.Impl
import           Data.Typeable (Typeable)
import           Foreign.C.Types
import           Foreign.Ptr (IntPtr, WordPtr, Ptr, FunPtr)
import           Foreign.StablePtr (StablePtr)
import           Foreign.Storable (Storable)
import           GHC.Real (Ratio)
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany.Internal (TypeclassInstance(..), getInstances, unAppsT)
import           Language.Haskell.TH.Syntax (mkNameG_tc, mkNameG_v)
import           System.Posix.Types (Fd)

-- TODO: This pattern of "define this set of instances given some other
-- instances / values" could be libraryized similarly to
-- mgsloan/instance-templates.

-- TODO: Generate inline pragmas? Probably not necessary

deriveManyStoreFromStorable :: (Type -> Bool) -> Q [Dec]
deriveManyStoreFromStorable =
    deriveManyStoreFromStorableImpl . (\p x -> p x && not (isPtrType x))

isPtrType :: Type -> Bool
isPtrType (ConT n) = n `elem`
    [''IntPtr, ''WordPtr, ''Fd, ''CUIntPtr, ''CIntPtr]
isPtrType (AppT (ConT n) _) = n `elem`
    [ ''FunPtr, ''Ptr, ''StablePtr
    -- Bit of a special case. The issue is that the Storable instance
    -- throws errors when the denominator is 0.
    , ''Ratio
    ]
isPtrType (AppT (AppT (ConT n) _) _) = n `elem`
    [ ''Const
    ]
isPtrType _ = False

deriveManyStoreFromStorableImpl :: (Type -> Bool) -> Q [Dec]
deriveManyStoreFromStorableImpl p = do
    -- Filters out overlapping instances and instances with more than
    -- one type arg (should be impossible).
    let postprocess = M.mapMaybeWithKey $ \tys xs ->
            case (tys, xs) of
                ([_ty], [x]) -> Just x
                _ -> Nothing
    storables <- postprocess . instancesMap <$> getInstances ''Storable
    stores <- postprocess . instancesMap <$> getInstances ''Store
    return $ M.elems $ flip M.mapMaybe (storables `M.difference` stores) $ \(TypeclassInstance context ty _) ->
        let argTy = head (tail (unAppsT ty)) in
        if p argTy
            then Just $ makeStoreInstance (addTypeableForStorable context)
                                          argTy
                                          'sizeStorable
                                          'peekStorable
                                          'pokeStorable
            else Nothing

-- Necessitated by the Typeable constraint on pokeStorable
addTypeableForStorable :: Cxt -> Cxt
addTypeableForStorable = concatMap go
  where
    go t@(AppT (ConT ((== ''Storable) -> True)) ty) = [t, AppT (ConT ''Typeable) ty]
    go t = [t]

makeStoreInstance :: Cxt -> Type -> Name -> Name -> Name -> Dec
makeStoreInstance context ty sizeName peekName pokeName =
    InstanceD context
              (AppT (ConT ''Store) ty)
              [ ValD (VarP 'size) (NormalB (VarE sizeName)) []
              , PragmaD (InlineP 'size Inline FunLike AllPhases)
              , ValD (VarP 'peek) (NormalB (VarE peekName)) []
              , PragmaD (InlineP 'peek Inline FunLike AllPhases)
              , ValD (VarP 'poke) (NormalB (VarE pokeName)) []
              , PragmaD (InlineP 'poke Inline FunLike AllPhases)
              ]

makeTupleInstance :: Int -> Dec
makeTupleInstance n =
    makeGenericInstance (map (AppT (ConT ''Store)) tvs)
                        (foldl1 AppT (TupleT n : tvs))
  where
    tvs = take n (map (VarT . mkName . (:[])) ['a'..'z'])

makeGenericInstance :: Cxt -> Type -> Dec
makeGenericInstance context ty = InstanceD context (AppT (ConT ''Store) ty) []

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
storeName = mkNameG_tc storePkg "Data.Store.Impl" "Store"
sizeStorableName = mkNameG_v storePkg "Data.Store.Impl" "sizeStorable"
peekStorableName = mkNameG_v storePkg "Data.Store.Impl" "peekStorable"
pokeStorableName = mkNameG_v storePkg "Data.Store.Impl" "pokeStorable"

-- Misc Util

appsT :: [TypeQ] -> TypeQ
appsT [] = error "appsE []"
appsT [x] = x
appsT (x:y:zs) = appsT ( (appT x y) : zs )
