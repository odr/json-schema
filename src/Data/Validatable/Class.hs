module Data.Validatable.Class(Validatable(..)) where

import Data.Foldable as F
import Data.Monoid
import Data.Proxy
import Data.RecInfo.FoldFields
import Data.RecInfo.RecFields
import Data.Tagged
import Data.Text as T
import Data.Validatable.Predicate
import GHC.TypeLits


class Validatable a where
  validate :: a -> Bool
  default validate :: FoldFields (RecFields a) Validatable a All => a -> Bool
  validate = getAll . foldFields @(RecFields a) @Validatable (All . validate)

instance Validatable Int
  where validate _ = True

instance Validatable Integer
  where validate _ = True

instance Validatable Bool
  where validate _ = True

instance Validatable Char
  where validate _ = True

instance Validatable a => Validatable (Maybe a) where
  validate = F.all validate
instance Validatable a => Validatable [a] where
  validate = F.all validate

instance Validatable (Tagged p a) => Validatable (Tagged (PNot p) a) where
  validate = not . validate @(Tagged p a) . retag

instance (Validatable (Tagged p1 a), Validatable (Tagged p2 a))
  => Validatable (Tagged (PAnd p1 p2) a) where
  validate = (&&)
    <$> validate @(Tagged p1 a) . retag
    <*> validate @(Tagged p2 a) . retag

instance (Validatable (Tagged p1 a), Validatable (Tagged p2 a))
  => Validatable (Tagged (POr p1 p2) a) where
  validate = (||)
    <$> validate @(Tagged p1 a) . retag
    <*> validate @(Tagged p2 a) . retag

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged (NLe n) a) where
  validate = compNum @n (<=)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged (NLt n) a) where
  validate = compNum @n (<)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged (NGe n) a) where
  validate = compNum @n (>=)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged (NGt n) a) where
  validate = compNum @n (>)

instance (Num a, KnownNat n, Ord a)
  => Validatable (Tagged (NEq n) a) where
  validate = compNum @n (==)

instance KnownNat n => Validatable (Tagged (TLe n) Text) where
  validate = compLen @n (<=)

instance KnownNat n => Validatable (Tagged (TLt n) Text) where
  validate = compLen @n (<)

instance KnownNat n => Validatable (Tagged (TGe n) Text) where
  validate = compLen @n (>=)

instance KnownNat n => Validatable (Tagged (TGt n) Text) where
  validate = compLen @n (>)

instance KnownNat n => Validatable (Tagged (TEq n) Text) where
  validate = compLen @n (==)

compNum
  :: forall n t a. (Num a, KnownNat n, Ord a)
  => (a -> a -> Bool) -> Tagged t a -> Bool
compNum p = (flip p (fromInteger $ natVal (Proxy @n))) . untag

compLen
  :: forall n t. KnownNat n
  => (Int -> Int -> Bool) -> Tagged t Text -> Bool
compLen p = (flip p (fromInteger $ natVal (Proxy @n))) . T.length . untag

-- instance KnownSymbol s => Validatable (Tagged (TRegex s) Text) where
--   validate =

instance Names xs => Validatable (Tagged (TEnum xs) Text) where
  validate = (`elem` names @xs) . untag

class Names (xs :: [Symbol]) where
  names :: [Text]
instance Names '[] where
  names = []
instance (KnownSymbol x, Names xs) => Names (x ': xs) where
  names = pack (symbolVal (Proxy @x)) : names @xs
