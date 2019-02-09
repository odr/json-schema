module Data.RecInfo.TH where

import Data.RecInfo.RecFields
import Language.Haskell.TH


genRecInfo :: Name -> DecsQ
genRecInfo rn = do
  fs <- reify rn >>= \case
    TyConI (DataD _ _ _ _ [RecC _ fs] _) -> pure fs
    TyConI (NewtypeD _ _ _ _ (RecC _ fs) _) -> pure fs
    x -> do
      reportError
        $ "genRecInfo: Only records with one constructor are supported.\n\
          \  Invalid pattern in reify: \n" ++ show x
      pure []
  recFieldsInst . toPromotedList $ getFieldInfo <$> fs
  where
    getFieldInfo (n,_,t) =
      AppT (AppT (PromotedTupleT 2) (LitT (StrTyLit $ nameBase n))) t
    recFieldsInst t = [d| type instance RecFields $(conT rn) = $(pure t) |]

toPromotedList :: [Type] -> Type
toPromotedList = foldr (\x xs -> AppT (AppT PromotedConsT x) xs) PromotedNilT
