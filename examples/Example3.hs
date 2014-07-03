module Main where

import Numeric.LBFGSB
import Numeric.LBFGSB.Convenience
import Numeric.LBFGSB.Result
import qualified Data.Vector.Storable as V

-- 2D not-everywhere-differentiable Rosenbrock function with approximate derivatives.

rosenbrock :: V.Vector Double -> Double
rosenbrock x = (1 - x0)^2 + abs (x1-x0^2)
    where
      x0 = x V.! 0
      x1 = x V.! 1

main :: IO ()
main = putStrLn "Testing with not-everywhere-differentiable Rosenbrock function, both unbounded and bounded with minimum outside bounds." >>
       let
           resUnbounded = minimize 5 1e1 1e-9 (Just 100) [] (V.fromList [100, 100]) rosenbrock (approximateGradient 1e-12 rosenbrock)
           resBounded = minimize 5 1e1 1e-9 (Just 100) [(Nothing, Nothing), (Nothing, Just 0.1)] (V.fromList [0, 0]) rosenbrock (approximateGradient 1e-12 rosenbrock)
       in
       putStrLn "Unbounded:" >>
       print resUnbounded >>
       putStrLn "-------------------" >>
       putStrLn "Bounded to (-infty, 0.1) in the second direction only:" >>
       print resBounded