module Data.Validatable.Predicate where

import Data.Text as T
import GHC.TypeLits


data ConsType = NumberConstraint | TextConstraint | ArrayConstraint

data Predicate (r::ConsType) where
  NLe    :: Nat -> Predicate NumberConstraint
  NLt    :: Nat -> Predicate NumberConstraint
  NGe    :: Nat -> Predicate NumberConstraint
  NGt    :: Nat -> Predicate NumberConstraint
  NEq    :: Nat -> Predicate NumberConstraint
  TLe    :: Nat -> Predicate TextConstraint
  TLt    :: Nat -> Predicate TextConstraint
  TGe    :: Nat -> Predicate TextConstraint
  TGt    :: Nat -> Predicate TextConstraint
  TEq    :: Nat -> Predicate TextConstraint
  TRegex :: Symbol -> Predicate TextConstraint
  TEnum  :: [Symbol] -> Predicate TextConstraint
  AEq    :: Nat -> Predicate ArrayConstraint
  PNot   :: Predicate r -> Predicate r
  PAnd   :: Predicate r -> Predicate r -> Predicate r
  POr    :: Predicate r -> Predicate r -> Predicate r
