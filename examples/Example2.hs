module Main where

import Numeric.LBFGSB
import Numeric.LBFGSB.Convenience
import Numeric.LBFGSB.Result
import qualified Data.Vector.Storable as V

-- 2D Rosenbrock function with approximate derivatives.

rosenbrock :: V.Vector Double -> Double
rosenbrock x = (1 - x0)^2 + 100*(x1-x0^2)^2
    where
      x0 = x V.! 0
      x1 = x V.! 1

main :: IO ()
main = putStrLn "Testing with Rosenbrock function, both unbounded and bounded with minimum outside bounds." >>
       let
           resUnbounded = minimize 5 1e0 1e-12 (Just 100) [] (V.fromList [100, 100]) rosenbrock (approximateGradient 1e-6 rosenbrock)
           resBounded = minimize 5 1e0 1e-12 (Just 100) [(Nothing, Nothing), (Nothing, Just 0.1)] (V.fromList [0, 0]) rosenbrock (approximateGradient 1e-6 rosenbrock)
       in
       putStrLn "Unbounded:" >>
       print resUnbounded >>
       putStrLn "-------------------" >>
       putStrLn "Bounded to (-infty, 0.1) in the second direction only:" >>
       print resBounded