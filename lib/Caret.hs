{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}

module Caret where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)

import Numeric.LinearAlgebra
import Caret.BFGS
import Numeric.GSL.Minimization
import Debug.Trace


type family Approx a

type instance Approx Bool = Double
type instance Approx Double = Double

data Caret a b = Caret
  { train :: [(Vector Double, b)] -> a
  , predict :: a -> Vector Double -> Approx b
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

--http://www.stat.cmu.edu/~cshalizi/350/lectures/26/lecture-26.pdf
logistic :: Caret (Vector Double) Bool
logistic = Caret t p where
  t the_data =  doMinNM the_data
  p beta x = 1/(1+exp (negate $ beta `dot` x))
  doMinNM the_data =
     fst $ minimizeV NMSimplex 1e-2 200 (getBox the_data) (logLike the_data) (getInit the_data)
  logLike1 beta (x, y) = let dp = beta `dot` x
                             pval = p beta x
                         in ind y * log pval + (1-ind y) * log 1 - pval
                            --log 1 + exp dp + ind y * dp
  logLike the_data beta =  negate $ sum $ map (logLike1 beta) the_data
  getInit the_data = VS.map (const 0.1) $ fst $ head the_data
  getBox the_data = VS.map (const 0.2) $ fst $ head the_data

prepare :: [a->Double] -> (a -> b) -> a -> (Vector Double, b)
prepare preds out x = (predictors preds x , out x)

predictors :: [a->Double] -> a -> Vector Double
predictors preds x = VS.fromList $ map ($x) preds

ind :: Bool -> Double
ind True = 1
ind False = 0
