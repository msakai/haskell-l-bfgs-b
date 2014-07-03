module Main where

import Numeric.LBFGSB
import Numeric.LBFGSB.Result
import qualified Data.Vector.Storable as V

-- Example optimization problem taken from driver1.f in the tarball
-- for version 3.0 of L-BFGS-B itself.

n :: Int
n = 25

f :: V.Vector Double -> Double
f x = 4* (V.foldl (\s i -> s + (x V.! i - (x V.! (i-1))^2)^2) (0.25* (x V.! 0 - 1)^2) (V.enumFromN 1 (n-1)))

g :: V.Vector Double -> V.Vector Double
g x = V.generate n (\i -> 8*(t (i-1)) - 1.6e1*(x V.! i)*(t i))
    where
      t i
        | i == -1  = 0.25*(x V.! 0 - 1)
        | i == n-1 = 0
        | otherwise =  x V.! (i+1) - (x V.! i)^2

bounds :: [(Maybe Double, Maybe Double)]
bounds = map (\i -> if odd i then (Just 1e0, Just 1e2) else (Just (-1e2), Just 1e2)) [1..n]

start :: V.Vector Double
start = V.replicate n 3.0

main :: IO ()
main = putStrLn "Testing with function and parameters from driver1.f from L-BFGS-B 3.0 distribution archive." >>
       let
           res = minimize 5 1e7 1e-5 Nothing bounds start f g
       in
         putStrLn "Full results:" >>
         print res >>
         putStrLn "Solution point:" >>
         print (solution res) >>
         putStrLn "Steps needed:" >>
         print (length (backtrace res)) >>
         putStrLn "Function value at solution:" >>
         print (f (solution res))
