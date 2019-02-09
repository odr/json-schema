module Main where

import Data.RecInfo.TH
import Data.Tagged
import Data.Text as T
import Data.Validatable.Class
import Data.Validatable.Predicate


data T1 = T1
  { t11 :: Integer
  , t12 :: Tagged (PAnd (NGt 10) (NLt 100)) Int
  , t13 :: Maybe (Tagged (TLt 10) Text) }
  deriving (Show, Eq)

genRecInfo ''T1

instance Validatable T1

main :: IO ()
main = do
  print $ validate (123 :: Integer)
  print $ validate $ T1 123 (Tagged 15) Nothing
  print $ validate $ T1 123 (Tagged 15) (Just $ Tagged "qwerqwer")
  print $ not $ validate $ T1 123 (Tagged 15) (Just $ Tagged "qwerqwer14234")
  print $ not $ validate $ T1 123 (Tagged 150) (Just $ Tagged "qwerqwer")
  print $ not $ validate $ T1 123 (Tagged 5) (Just $ Tagged "qwerqwer")
