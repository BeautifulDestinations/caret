module Caret.GenData where

import Data.Random
import Data.Random.Distribution.Normal
import Data.Random.Distribution.Bernoulli

data Person = Person
  { height :: Double
  , weight :: Double
  , is_male :: Bool
  , heart_attack :: Bool
  } deriving Show

personDist :: RVar Person
personDist = do
  is_male <- bernoulli (0.5::Double)
  height <- if is_male
              then normal 180 15
              else normal 165 15
  weight <- normal (height / 2.5) 10
  heart_attack <- bernoulli $ 0.2 + 0.6 * sigmoid (weight / height - 2.5)
  return $ Person height weight is_male heart_attack

genPersons :: Int -> IO [Person]
genPersons n = do
  sample $ sequence $ replicate n personDist

sigmoid t = 1/(1+exp(-t))
