module Data.RecInfo.RecFields where

import Data.Kind
import GHC.TypeLits


type family RecFields r :: [(Symbol,Type)]
