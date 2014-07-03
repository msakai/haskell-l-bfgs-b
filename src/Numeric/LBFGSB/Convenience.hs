{-# LANGUAGE FlexibleContexts #-}

-- | Some functions that can be useful together with L-BFGS-B.
module Numeric.LBFGSB.Convenience(approximateGradient, listFunction, vectorFunction) where

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Generic as GV

-- | @'approximateGradient' h f x@ is an approximation of the gradient
-- of @f@ at @x@, computed using central differences with step size
-- @h@.
approximateGradient :: (GV.Vector v Double) => Double -> (v Double -> Double) -> (v Double -> v Double)
approximateGradient h f x = GV.generate (GV.length x) (\i -> central h (\y -> f (replaceAt i y x)) (x GV.! i))

-- | Turn a function on lists into one on 'V.Storable' 'V.Vector's.
listFunction :: ([Double] -> Double) -> (V.Vector Double -> Double)
listFunction f = f . V.toList

-- | Turn a function on any generic 'GV.Vector's into one on 'V.Storable' 'V.Vector's.
vectorFunction :: (GV.Vector v Double) => (v Double -> Double) -> (V.Vector Double -> Double)
vectorFunction f = f . GV.convert

replaceAt :: (GV.Vector v a) => Int -> a -> v a -> v a
replaceAt n x xs
    | n > GV.length xs - 1 || n < 0 = xs
    | otherwise                     = xs GV.// [(n, x)]

central :: Double -> (Double -> Double) -> Double -> Double
central h f x = (f (x+h) - f (x-h))/(2*h)