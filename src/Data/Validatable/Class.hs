module Data.Validatable.Class(Validatable(..)) where

import Data.Foldable as F
import Data.Monoid
import Data.RecInfo.FoldFields
import Data.RecInfo.RecFields


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
