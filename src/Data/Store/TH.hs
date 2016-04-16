{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MagicHash #-}

module Data.Store.TH
    (
    -- * TH functions for generating Store instances
      deriveManyStoreFromStorable
    , makeTupleStoreInstance
    , deriveManyStorePrimVector
    -- * TH functions for testing Store instances with hspec + smallcheck
    , smallcheckManyStore
    -- * Misc utilties used in Store test
    , getAllInstanceTypes1
    , isMonoType
    ) where

import           Control.Applicative
import           Data.Complex ()
import           Data.Generics.Schemes (listify)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Primitive.ByteArray
import           Data.Primitive.Types
import           Data.Store.Impl
import           Data.Traversable (forM)
import           Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Unboxed as UV
import           Debug.Trace (trace)
import           Foreign.C.Types
import           Foreign.Ptr (IntPtr, WordPtr, Ptr, FunPtr)
import           Foreign.StablePtr (StablePtr)
import           Foreign.Storable (Storable)
import           GHC.Real (Ratio)
import           GHC.Types (Int(..))
import           Language.Haskell.TH
import           Language.Haskell.TH.ReifyMany.Internal (TypeclassInstance(..), getInstances, unAppsT)
import           Language.Haskell.TH.Syntax (lift)
import           Safe (headMay)
import           System.Posix.Types (Fd)
import           Test.Hspec
import           Test.Hspec.SmallCheck (property)
import           Test.SmallCheck

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

deriveManyStorePrimVector :: Q [Dec]
deriveManyStorePrimVector = do
    prims <- postprocess . instancesMap <$> getInstances ''PV.Prim
    stores <- postprocess . instancesMap <$> getInstances ''Store
    let primInsts =
            M.mapKeys (map (AppT (ConT ''PV.Vector))) prims
            `M.difference`
            stores
    forM (M.toList primInsts) $ \([instTy], TypeclassInstance cs ty _) -> do
        let argTy = head (tail (unAppsT ty))
        sizeExpr <- [|
            VarSize $ \x ->
                I# $(primSizeOfExpr (ConT ''Int)) +
                I# $(primSizeOfExpr argTy) * PV.length x
            |]
        peekExpr <- [| do
            len <- peek
            let sz = I# $(primSizeOfExpr argTy)
            array <- peekByteArray $(lift ("Primitive Vector (" ++ pprint argTy ++ ")"))
                                   (len * sz)
            return (PV.Vector 0 len array)
            |]
        pokeExpr <- [| \(PV.Vector offset len (ByteArray array)) -> do
            let sz = I# $(primSizeOfExpr argTy)
            poke len
            pokeByteArray array (offset * sz) (len * sz)
            |]
        return $ makeStoreInstance cs instTy sizeExpr peekExpr pokeExpr


primSizeOfExpr :: Type -> ExpQ
primSizeOfExpr ty = [| $(varE 'sizeOf#) (error "sizeOf# evaluated its argument" :: $(return ty)) |]

{-
deriveManyStoreUnboxVector :: Q [Dec]
deriveManyStoreUnboxVector = do
    unboxes <- getUnboxInfo
    stores <- postprocess . instancesMap <$> getInstances ''Store
    let unboxInsts = M.elems $
            M.toList (map (\(ty, con, args) -> (AppT ''UV.Vector ty, (ty, con, args))) unboxes)
            `M.difference`
            stores
    results <- forM unboxInsts $ \(ty, con, args) ->
        [e| let !($(conP con ))

    return $ M.elems $ flip M.mapMaybe () $
        \(TypeclassInstance cs ty _) ->
            let argTy = head (tail (unAppsT ty)) in
            Just $ makeStoreInstance cs argTy
                (AppE (VarE 'pokeUnboxedVector) (proxyEx)
-}

getUnboxInfo :: Q [(Type, Name, [Type])]
getUnboxInfo = do
    FamilyI _ insts <- reify ''UV.Vector
    return (map go insts)
  where
    go (NewtypeInstD cxt _ [ty] con _) =
        (ty, conName con, conFields con)
    go (DataInstD cxt _ [ty] [con] _) =
        (ty, conName con, conFields con)
    go x = error ("Unexpected result from reifying Unboxed Vector instances: " ++ pprint x)

conName :: Con -> Name
conName (NormalC name _) = name
conName (RecC name _) = name
conName (InfixC _ name _) = name
conName (ForallC _ _ con) = conName con

conFields :: Con -> [Type]
conFields (NormalC _ tys) = map snd tys
conFields (RecC _ tys) = map (\(_,_,ty) -> ty) tys
conFields (InfixC l _ r) = [snd l, snd r]
conFields (ForallC _ _ con) = conFields con

{-
pokeUnboxedVector =
    let !(UV.V_Word (PV.Vector offset len (ByteArray array))) = x
    sz = sizeOf (undefined :: Word)
    poke len
    pokeByteArray array (offset * sz) (len * sz)

proxyExpr :: Type -> Exp
proxyExpr ty = SigE 'Proxy (AppT ''Proxy ty)
-}

deriveManyStoreFromStorableImpl :: (Type -> Bool) -> Q [Dec]
deriveManyStoreFromStorableImpl p = do
    storables <- postprocess . instancesMap <$> getInstances ''Storable
    stores <- postprocess . instancesMap <$> getInstances ''Store
    return $ M.elems $ flip M.mapMaybe (storables `M.difference` stores) $
        \(TypeclassInstance cs ty _) ->
        let argTy = head (tail (unAppsT ty)) in
        if p argTy
            then Just $ makeStoreInstance (addTypeableForStorable cs)
                                          argTy
                                          (VarE 'sizeStorable)
                                          (VarE 'peekStorable)
                                          (VarE 'pokeStorable)
            else Nothing

-- Filters out overlapping instances and instances with more than one
-- type arg (should be impossible).
postprocess =
    M.mapMaybeWithKey $ \tys xs ->
        case (tys, xs) of
            ([_ty], [x]) -> Just x
            _ -> Nothing

-- Necessitated by the Typeable constraint on pokeStorable
addTypeableForStorable :: Cxt -> Cxt
addTypeableForStorable = concatMap go
  where
    go t@(AppT (ConT ((== ''Storable) -> True)) ty) = [t, AppT (ConT ''Typeable) ty]
    go t = [t]

makeStoreInstance :: Cxt -> Type -> Exp -> Exp -> Exp -> Dec
makeStoreInstance cs ty sizeExpr peekExpr pokeExpr =
    InstanceD cs
              (AppT (ConT ''Store) ty)
              [ ValD (VarP 'size) (NormalB sizeExpr) []
              , PragmaD (InlineP 'size Inline FunLike AllPhases)
              , ValD (VarP 'peek) (NormalB peekExpr) []
              , PragmaD (InlineP 'peek Inline FunLike AllPhases)
              , ValD (VarP 'poke) (NormalB pokeExpr) []
              , PragmaD (InlineP 'poke Inline FunLike AllPhases)
              ]

makeTupleStoreInstance :: Int -> Dec
makeTupleStoreInstance n =
    makeGenericInstance (map (AppT (ConT ''Store)) tvs)
                        (foldl1 AppT (TupleT n : tvs))
  where
    tvs = take n (map (VarT . mkName . (:[])) ['a'..'z'])

makeGenericInstance :: Cxt -> Type -> Dec
makeGenericInstance cs ty = InstanceD cs (AppT (ConT ''Store) ty) []

smallcheckMany :: [Q (String, Exp)] -> ExpQ
smallcheckMany = doE . map (\f -> f >>= \(name, expr) -> noBindS [e| it name $ $(return expr) |])

smallcheckManyStore :: Bool -> Int -> [TypeQ] -> ExpQ
smallcheckManyStore verbose depth = smallcheckMany . map testRoundtrip
  where
    testRoundtrip tyq = do
        ty <- tyq
        expr <- [e| property $ changeDepth (\_ -> depth) $ \x ->
            let encoded = verboseTrace verbose "encoded" (encode x)
                decoded = verboseTrace verbose "decoded" (decode encoded)
             in decoded == Right (x :: $(return ty)) |]
        return ("Roundtrips (" ++ pprint ty ++ ")", expr)

verboseTrace :: Show a => Bool -> String -> a -> a
verboseTrace True msg x = trace (show (msg, x)) x
verboseTrace False _ x = x

-- TODO: either generate random types that satisfy instances with
-- variables in them, or have a check that there's at least a manual
-- check for polymorphic instances.

getAllInstanceTypes :: Name -> Q [[Type]]
getAllInstanceTypes n =
    map (\(TypeclassInstance _ ty _) -> drop 1 (unAppsT ty)) <$>
    getInstances n

getAllInstanceTypes1 :: Name -> Q [Type]
getAllInstanceTypes1 n =
    fmap (fmap (fromMaybe (error "getAllMonoInstances1 expected only one type argument") . headMay))
         (getAllInstanceTypes n)

isMonoType :: Type -> Bool
isMonoType = null . listify isVarT
  where
    isVarT VarT{} = True
    isVarT _ = False

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
