{-# LANGUAGE FunctionalDependencies #-}

module Caret where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)

import Numeric.LinearAlgebra

type Vec = Vector Double

data Caret a b = Caret
  { train :: [(Vector Double, b)] -> a
  , predict :: a -> Vector Double -> b
  }

ols :: Caret (Vector Double) Double
ols = Caret t p where
  t the_data = let a = fromRows $ map fst the_data
                   b = col $ map snd the_data
               in head $ toColumns $ linearSolveSVD a b
  p betas x = betas `dot` x

--ridge :: Double -> Caret (Vector Double) Double

--logistic :: Caret (Vector Double) Bool


newtype OlsParams = OlsParams { unOlsParams :: Vector Double }

class CaretC a b h | a -> b, a -> h where
  trainC :: h -> [(Vector Double, b)] -> a
  predictC :: a -> Vector Double -> b
