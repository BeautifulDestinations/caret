{-# LANGUAGE FunctionalDependencies, TypeFamilies #-}

module Caret where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)

import Numeric.LinearAlgebra
import Caret.BFGS

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
  t the_data = case bfgs (logLike the_data) (grad the_data) (getInit the_data) of
                 Left err -> error err
                 Right (p,_) -> p
  p beta x = 1/(1+exp (negate $ beta `dot` x))
  logLike1 beta (x, y) = let dp = beta `dot` x
                         in -log 1 + exp dp + ind y * dp
  logLike the_data beta = negate $ sum $ map (logLike1 beta) the_data
  grad1 beta (x,y)  = VS.map (\xij -> (ind y - recip (1+exp(negate $ beta `dot`x))) * xij) x
  grad the_data beta = VS.map negate $ foldl1 (VS.zipWith (+)) $ map (grad1 beta) the_data
  getInit the_data = VS.map (const 0) $ fst $ head the_data



newtype OlsParams = OlsParams { unOlsParams :: Vector Double }

class CaretC a b h | a -> b, a -> h where
  trainC :: h -> [(Vector Double, b)] -> a
  predictC :: a -> Vector Double -> b

prepare :: [a->Double] -> (a -> b) -> a -> (Vector Double, b)
prepare preds out x = (VS.fromList $ map ($x) preds , out x)

ind :: Bool -> Double
ind True = 1
ind False = 1
