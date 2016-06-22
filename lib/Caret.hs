{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}

module Caret where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)

import Numeric.LinearAlgebra


data Caret a b = Caret
  { train :: [(Vector Double, b)] -> a
  , predict :: a -> Vector Double -> b
  }

ols :: Caret (Vector Double) Double
ols = Caret t p where
  t the_data = let x = fromRows $ map fst the_data
                   y = col $ map snd the_data
               in head $ toColumns $ inv (tr' x <> x) <> tr' x <> y
  p betas x = betas `dot` x

ridge :: Double -> Caret (Vector Double) Double
ridge alpha = Caret t p where
  t the_data = let x = fromRows $ map fst the_data
                   y = col $ map snd the_data
                   npars = VS.length $ fst $ head the_data
                   gamma = scale alpha $ ident npars
               in head $ toColumns $ inv (tr' x <> x + tr' gamma <> gamma) <> tr' x <> y
  p betas x = betas `dot` x

logistic :: Caret (Vector Double) Bool
logistic = Caret t p where
  t the_data = undefined
  p betas x = undefined

--http://www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
--https://hackage.haskell.org/package/regress-0.1.1/docs/src/Numeric-Regression-Logistic.html#regress
--logistic :: Caret (Vector Double) Bool


newtype OlsParams = OlsParams { unOlsParams :: Vector Double }

class CaretC a b h | a -> b, a -> h where
  trainC :: h -> [(Vector Double, b)] -> a
  predictC :: a -> Vector Double -> b

prepare :: [a->Double] -> (a -> b) -> a -> (Vector Double, b)
prepare preds out x = (VS.fromList $ map ($x) preds , out x)

ind :: Bool -> Double
ind True = 1
ind False = 1
