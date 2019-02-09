module Data.Validatable.Predicate where

import Data.Proxy
import Data.Tagged
import Data.Text as T
import Data.Validatable.Class
import GHC.TypeLits


data ConsType = NumberConstraint | TextConstraint | ArrayConstraint

data Predicate (r::ConsType) where
  NLe    :: Nat -> Predicate 'NumberConstraint
  NLt    :: Nat -> Predicate 'NumberConstraint
  NGe    :: Nat -> Predicate 'NumberConstraint
  NGt    :: Nat -> Predicate 'NumberConstraint
  NEq    :: Nat -> Predicate 'NumberConstraint
  TLe    :: Nat -> Predicate 'TextConstraint
  TLt    :: Nat -> Predicate 'TextConstraint
  TGe    :: Nat -> Predicate 'TextConstraint
  TGt    :: Nat -> Predicate 'TextConstraint
  TEq    :: Nat -> Predicate 'TextConstraint
  TRegex :: Symbol -> Predicate 'TextConstraint
  TEnum  :: [Symbol] -> Predicate 'TextConstraint
  AEq    :: Nat -> Predicate 'ArrayConstraint
  PNot   :: Predicate r -> Predicate r
  PAnd   :: Predicate r -> Predicate r -> Predicate r
  POr    :: Predicate r -> Predicate r -> Predicate r

instance Validatable (Tagged p a) => Validatable (Tagged ('PNot p) a) where
  validate = not . validate @(Tagged p a) . retag

instance (Validatable (Tagged p1 a), Validatable (Tagged p2 a))
  => Validatable (Tagged ('PAnd p1 p2) a) where
  validate = (&&)
    <$> validate @(Tagged p1 a) . retag
    <*> validate @(Tagged p2 a) . retag

instance (Validatable (Tagged p1 a), Validatable (Tagged p2 a))
  => Validatable (Tagged ('POr p1 p2) a) where
  validate = (||)
    <$> validate @(Tagged p1 a) . retag
    <*> validate @(Tagged p2 a) . retag

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged ('NLe n) a) where
  validate = compNum @n (<=)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged ('NLt n) a) where
  validate = compNum @n (<)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged ('NGe n) a) where
  validate = compNum @n (>=)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged ('NGt n) a) where
  validate = compNum @n (>)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged ('NEq n) a) where
  validate = compNum @n (==)

instance KnownNat n => Validatable (Tagged ('TLe n) Text) where
  validate = compLen @n (<=)

instance KnownNat n => Validatable (Tagged ('TLt n) Text) where
  validate = compLen @n (<)

instance KnownNat n => Validatable (Tagged ('TGe n) Text) where
  validate = compLen @n (>=)

instance KnownNat n => Validatable (Tagged ('TGt n) Text) where
  validate = compLen @n (>)

instance KnownNat n => Validatable (Tagged ('TEq n) Text) where
  validate = compLen @n (==)

compNum
  :: forall n t a. (Num a, KnownNat n, Ord a)
  => (a -> a -> Bool) -> Tagged t a -> Bool
compNum p = (`p` (fromInteger $ natVal (Proxy @n))) . untag

compLen
  :: forall n t. KnownNat n
  => (Int -> Int -> Bool) -> Tagged t Text -> Bool
compLen p = (`p` (fromInteger $ natVal (Proxy @n))) . T.length . untag

-- instance KnownSymbol s => Validatable (Tagged (TRegex s) Text) where
--   validate =
-- TODO: TRefex, AEq

instance Names xs => Validatable (Tagged ('TEnum xs) Text) where
  validate = (`elem` names @xs) . untag

class Names (xs :: [Symbol]) where
  names :: [Text]
instance Names '[] where
  names = []
instance (KnownSymbol x, Names xs) => Names (x ': xs) where
  names = pack (symbolVal (Proxy @x)) : names @xs
