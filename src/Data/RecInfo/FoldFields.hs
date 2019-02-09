module Data.RecInfo.FoldFields where

import Data.Kind
import GHC.Records
import GHC.TypeLits


class Monoid r
  => FoldFields (xs :: [(Symbol, Type)]) (c :: Type -> Constraint) v r where
  foldFields :: (forall x. c x => x -> r) -> v -> r

instance Monoid r => FoldFields '[] c v r where
  foldFields _ = const mempty

instance (HasField s v t, Monoid r, c t, FoldFields xs c v r)
  => FoldFields ( '(s,t) ': xs) c v r where
  foldFields f v = f (getField @s v) `mappend` foldFields @xs @c f v

-- class Monoid r => FoldTL (xs :: [k]) (c :: k -> Constraint) r where
--   foldTL :: (forall (x::k). c x => r) -> r
--
-- instance Monoid r => FoldTL '[] c r where
--   foldTL _ =  mempty
--
-- instance (Monoid r, FoldTL xs c r, c x) => FoldTL ((x::k) ': xs) c r where
--   foldTL f = f @x `mappend` foldTL @xs @c f
--                       the problem is here ^
