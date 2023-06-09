{-# LANGUAGE ForeignFunctionInterface #-}

-- | Minimize functions using the Fortran L-BFGS-B library for
-- limited-memory Broyden–Fletcher–Goldfarb–Shanno bound-constrained
-- minimization. More information on assumptions and function parameters can be found at the L-BFGS-B homepage <http://users.eecs.northwestern.edu/~nocedal/lbfgsb.html> and in its source code.
--
-- A /bound-constrained/ domain is one that is a finite product of the
-- reals, closed intervals, and half-infinite intervals. We describe
-- the factors by @('Maybe' 'Double', 'Maybe' 'Double')@, with
-- @('Just' a, 'Just' b)@ describing the closed interval [a,b], and so
-- forth.
module Numeric.LBFGSB(minimize, minimize') where

import Data.List
import Data.Maybe
import Control.Applicative
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils
import Foreign.ForeignPtr.Safe
import System.IO.Unsafe(unsafeDupablePerformIO)
import qualified Data.Vector.Storable as V
import qualified Numeric.LBFGSB.Result as R


-- | Minimization using L-BFGS-B. If you only require the solution
-- point, and not the full 'R.Result', see 'minimize''. Take care to
-- satisfy the requirements on the arguments. Failure to do so may result
-- in a runtime error, or, when noted, undefined behavior/crash.
minimize :: Int                                  -- ^ @m@: The maximum number of variable metric corrections used
                                                 -- to define the limited memory matrix. /Suggestion:/ @5@.
         -> Double                               -- ^ @factr@: Iteration stops when the relative change in function value
                                                 -- is smaller than @factr*eps@, where @eps@ is a measure of machine precision
                                                 -- generated by the Fortran code. @1e12@ is low accuracy, @1e7@ is moderate,
                                                 -- and @1e1@ is extremely high. Must be @>=1@. /Suggestion:/ @1e7@.
         -> Double                               -- ^ @pgtol@: Iteration stops when the largest component of the projected
                                                 -- gradient is smaller than @pgtol@. Must be @>=0@. /Suggestion:/ @1e-5@.
         -> Maybe Int                            -- ^ @'Just' steps@ means the minimization is aborted if it has not converged after 
                                                 -- @steps>0@ iterations. 'Nothing' signifies no limit.
         -> [(Maybe Double, Maybe Double)]       -- ^ Constraints, as described in the beginning of this module. If there are
                                                 -- fewer bounds than components in @x0@, the remaining dimensions are assumed
                                                 -- to be unbounded. @[]@ thus gives unbounded minimization. Moreover,
                                                 -- if there are more bounds than components in @x0@, only as many as needed are used.
                                                 -- @'repeat' ('Just' 0, 'Just' 1)@ thus specifies the unit cube of any dimension
                                                 -- as constraint.
         -> V.Vector Double                      -- ^ @x0@: Starting point. The point /must/ be within the bounds.
         -> (V.Vector Double -> Double)          -- ^ @f@: Function to minimize. /Must/ take 'V.Vector's of precisely the same
                                                 -- length as @x0@, or else behavior is undefined (the program may crash)!
         -> (V.Vector Double -> V.Vector Double) -- ^ @g@: Gradient of @f@. /Must/ take and return 'V.Vector's of precisely the same
                                                 -- length as @x0@, or else behavior is undefined (the program may crash)!
                                                 -- "Numeric.LBFGSB.Convenience" provides a simple approximation of the gradient if
                                                 -- you do not have the real one.
         -> R.Result
minimize m factr pgtol steps bounds x0 f g 
    | not (factr >= 0) = error "factr must be >=0."
    | not (pgtol >= 0) = error "pgtol must be >=0."
    | not (maybe True (>0) steps) = error "steps must be >0."
    | not (inBounds bounds' x0) = error "x0 must be within bounds."
    | otherwise = unsafeDupablePerformIO (runDriver m factr pgtol steps bounds' x0 f g)
    where
      bounds' = take (V.length x0) (bounds ++ repeat (Nothing, Nothing))

-- | If L-BFGS-B converges within the specified number of steps, the
-- solution point is returned as @'Just' solution@. Otherwise
-- 'Nothing' is returned. The arguments are the same as for
-- 'minimize'.
minimize' :: Int 
          -> Double 
          -> Double 
          -> Maybe Int
          -> [(Maybe Double, Maybe Double)] 
          -> V.Vector Double 
          -> (V.Vector Double -> Double) 
          -> (V.Vector Double -> V.Vector Double) 
          -> Maybe (V.Vector Double)
minimize' m factr pgtol steps bounds x0 f g 
    | R.stopReason result == R.Converged = Just $ R.solution result
    | otherwise                          = Nothing
    where
      result = minimize m factr pgtol steps bounds x0 f g

inBounds :: [(Maybe Double, Maybe Double)] -> V.Vector Double -> Bool
inBounds bounds v = all ib (zip bounds (V.toList v))
    where
      ib ((Nothing, Nothing), _) = True
      ib ((Nothing, Just u),  x) = x <= u
      ib ((Just l , Nothing), x) = x >= l
      ib ((Just l , Just u),  x) = x >= l && x <= u

data DriverContext = DriverContext { pn :: Ptr CInt
                                   , pm :: Ptr CInt
                                   , px :: Ptr Double
                                   , pl :: Ptr Double
                                   , pu :: Ptr Double
                                   , pnbd :: Ptr CInt
                                   , pf :: Ptr Double
                                   , pg :: Ptr Double
                                   , pfactr :: Ptr Double
                                   , ppgtol :: Ptr Double
                                   , pwa :: Ptr Double
                                   , piwa :: Ptr CInt
                                   , ptask :: Ptr CChar
                                   , piprint :: Ptr CInt
                                   , pcsave :: Ptr CChar
                                   , plsave :: Ptr CInt
                                   , pisave :: Ptr CInt
                                   , pdsave :: Ptr Double }

data TaskPrefix = FG | NewX | Start | Convergence | Other String
                  deriving (Eq, Show)

stringToTaskPrefix :: String -> TaskPrefix
stringToTaskPrefix s
    | "FG" `isPrefixOf` s = FG
    | "NEW_X" `isPrefixOf` s = NewX
    | "START" `isPrefixOf` s = Start
    | "CONVERGENCE" `isPrefixOf` s = Convergence
    | otherwise = Other s

taskLength :: Int
taskLength = 60

csaveLength :: Int
csaveLength = 60

startString :: String
startString = take taskLength ("START" ++ repeat ' ')

unzipBounds :: [(Maybe Double, Maybe Double)] -> ([Double], [Double], [CInt])
unzipBounds bounds = unzip3 (map helper bounds)
    where
      helper (Nothing, Nothing) = (0, 0, 0)
      helper (Just l, Nothing) = (l, 0, 1)
      helper (Just l, Just u) = (l, u, 2)
      helper (Nothing, Just u) = (0, u, 3)

runDriver :: Int 
          -> Double
          -> Double
          -> Maybe Int
          -> [(Maybe Double, Maybe Double)]
          -> V.Vector Double
          -> (V.Vector Double -> Double)
          -> (V.Vector Double -> V.Vector Double)
          -> IO R.Result
runDriver m factr tol steps bounds x0 f g 
    = let
         n = V.length x0
         (ls, us, bds) = unzipBounds bounds
      in   
      with (fromIntegral n) $ \pn ->
      with (fromIntegral m) $ \pm ->
      mallocForeignPtrArray n >>= \fpx -> 
      withForeignPtr fpx $ \px ->
      withForeignPtr ((fst . V.unsafeToForeignPtr0) x0) (\px0 -> copyArray px px0 n) >>= \_ ->
      withArray ls $ \pl ->
      withArray us $ \pu ->
      withArray bds $ \pnbd ->
      alloca $ \pf ->
      allocaArray n $ \pg ->
      with factr $ \pfactr ->
      with tol $ \ppgtol ->
      allocaArray (2*m*n + 11*m*m + 5*n + 8*m) $ \pwa ->
      allocaArray (3*n) $ \piwa ->
      allocaArray taskLength $ \ptask ->
      withCAString startString (\pstart -> copyArray ptask pstart taskLength) >>= \_ ->
      with ((-1) :: CInt) $ \piprint ->
      allocaArray csaveLength $ \pcsave ->
      allocaArray 4 $ \plsave ->
      allocaArray 44 $ \pisave ->
      allocaArray 29 $ \pdsave ->
      driver (DriverContext pn pm px pl pu pnbd pf pg pfactr ppgtol pwa piwa ptask piprint pcsave plsave pisave pdsave) steps [] f g >>= \(btrace, stopReason) ->
      return (R.Result (V.unsafeFromForeignPtr0 fpx n) btrace stopReason)

driver :: DriverContext 
       -> Maybe Int
       -> [V.Vector Double] 
       -> (V.Vector Double -> Double) 
       -> (V.Vector Double -> V.Vector Double) 
       -> IO ([V.Vector Double], R.StopReason)
driver context stepsLeft backtrace f g
       = readTaskPrefix context >>= \task ->
         case task of
           Convergence -> return (backtrace, R.Converged)
           Start -> makeCall context >> 
                    driver context stepsLeft backtrace f g
           FG -> readX context >>= \x ->
                 update context f g x >>
                 makeCall context >>
                 driver context stepsLeft backtrace f g
           NewX -> readX context >>= \x -> 
                   makeCall context >>
                   if maybe True (> 0) stepsLeft
                   then driver context (Just (subtract 1) <*> stepsLeft) (x:backtrace) f g
                   else return (backtrace, R.StepCount)
           Other s -> return (backtrace, R.Other s)

update :: DriverContext -> (V.Vector Double -> Double) -> (V.Vector Double -> V.Vector Double) -> V.Vector Double -> IO ()
update context f g x
    = poke (pf context) (f x) >>
      V.unsafeWith (g x) (\pgx -> copyArray (pg context) pgx (V.length x))

readX :: DriverContext ->  IO (V.Vector Double)
readX context
    = peek (pn context) >>= (return . fromIntegral) >>= \n ->
      mallocForeignPtrArray n >>= \fpxCopy ->
      withForeignPtr fpxCopy (\pxCopy -> copyArray pxCopy (px context) n) >>  -- fpxCopy is a foreign pointer to a *copy* of the current x
      return (V.unsafeFromForeignPtr0 fpxCopy n)

readTaskPrefix :: DriverContext -> IO TaskPrefix
readTaskPrefix context = peekCStringLen (ptask context, taskLength) >>= (return . stringToTaskPrefix)
                      

makeCall :: DriverContext -> IO ()
makeCall context@(DriverContext pn pm px pl pu pnbd pf pg pfactr ppgtol pwa piwa ptask piprint pcsave plsave pisave pdsave)
    = fortran_setulb pn pm px pl pu pnbd pf pg pfactr ppgtol pwa piwa ptask piprint pcsave plsave pisave pdsave 

foreign import ccall "setulb_"
        fortran_setulb     -- #   Comments below are from the L-BFGS-B Fortran source code:
            :: Ptr CInt    --1    n is an integer variable.
                           --       On entry n is the dimension of the problem.
                           --       On exit n is unchanged.
            -> Ptr CInt    --2    m is an integer variable.
                           --       On entry m is the maximum number of variable metric corrections
                           --       used to define the limited memory matrix.
                           --       On exit m is unchanged.
            -> Ptr Double  --3    x is a double precision array of dimension n.
                           --       On entry x is an approximation to the solution.
                           --       On exit x is the current approximation.
            -> Ptr Double  --4    l is a double precision array of dimension n.
                           --       On entry l is the lower bound on x.
                           --       On exit l is unchanged.
            -> Ptr Double  --5    u is a double precision array of dimension n.
                           --       On entry u is the upper bound on x.
                           --       On exit u is unchanged.
            -> Ptr CInt    --6    nbd is an integer array of dimension n.
                           --       On entry nbd represents the type of bounds imposed on the
                           --         variables, and must be specified as follows:
                           --         nbd(i)=0 if x(i) is unbounded,
                           --                1 if x(i) has only a lower bound,
                           --                2 if x(i) has both lower and upper bounds, and
                           --                3 if x(i) has only an upper bound.
                           --       On exit nbd is unchanged.
            -> Ptr Double  --7    f is a double precision variable.
                           --       On first entry f is unspecified.
                           --       On final exit f is the value of the function at x.
            -> Ptr Double  --8    g is a double precision array of dimension n.
                           --       On first entry g is unspecified.
                           --       On final exit g is the value of the gradient at x.
            -> Ptr Double  --9    factr is a double precision variable.
                           --       On entry factr >= 0 is specified by the user.  The iteration
                           --         will stop when
                           --         (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
                           --         where epsmch is the machine precision, which is automatically
                           --         generated by the code. Typical values for factr: 1.d+12 for
                           --         low accuracy; 1.d+7 for moderate accuracy; 1.d+1 for extremely
                           --         high accuracy.
                           --       On exit factr is unchanged.
            -> Ptr Double  --10   pgtol is a double precision variable.
                           --       On entry pgtol >= 0 is specified by the user.  The iteration
                           --         will stop when
                           --         max{|proj g_i | i = 1, ..., n} <= pgtol
                           --         where pg_i is the ith component of the projected gradient.   
                           --       On exit pgtol is unchanged.
            -> Ptr Double  --11   wa is a double precision working array of length                  -- In version 3.0:
                           --       (2mmax + 5)nmax + 12mmax^2 + 12mmax.                            -- 2*m*n + 11m*m + 5*n + 8*m
            -> Ptr CInt    --12   iwa is an integer working array of length 3nmax.
            -> Ptr CChar   --13   task is a working string of characters of length 60 indicating
                           --       the current job when entering and quitting this subroutine.
            -> Ptr CInt    --14   iprint is an integer variable that must be set by the user.
                           --       It controls the frequency and type of output generated:
                           --        iprint<0    no output is generated;
                           --        iprint=0    print only one line at the last iteration;
                           --        0<iprint<99 print also f and |proj g| every iprint iterations;
                           --        iprint=99   print details of every iteration except n-vectors;
                           --        iprint=100  print also the changes of active set and final x;
                           --        iprint>100  print details of every iteration including x and g;
                           --       When iprint > 0, the file iterate.dat will be created to
                           --                        summarize the iteration.
            -> Ptr CChar   --15   csave is a working string of characters of length 60.
            -> Ptr CInt    --16   lsave is a logical working array of dimension 4.
                           --       On exit with 'task' = NEW_X, the following information is 
                           --                                                             available:
                           --         If lsave(1) = .true.  then  the initial X has been replaced by
                           --                                     its projection in the feasible set;
                           --         If lsave(2) = .true.  then  the problem is constrained;
                           --         If lsave(3) = .true.  then  each variable has upper and lower
                           --                                     bounds;
            -> Ptr CInt    --17   isave is an integer working array of dimension 44.
                           --       On exit with 'task' = NEW_X, the following information is 
                           --                                                             available:
                           --         isave(22) = the total number of intervals explored in the 
                           --                         search of Cauchy points;
                           --         isave(26) = the total number of skipped BFGS updates before 
                           --                         the current iteration;
                           --         isave(30) = the number of current iteration;
                           --         isave(31) = the total number of BFGS updates prior the current
                           --                         iteration;
                           --         isave(33) = the number of intervals explored in the search of
                           --                         Cauchy point in the current iteration;
                           --         isave(34) = the total number of function and gradient 
                           --                         evaluations;
                           --         isave(36) = the number of function value or gradient
                           --                                  evaluations in the current iteration;
                           --         if isave(37) = 0  then the subspace argmin is within the box;
                           --         if isave(37) = 1  then the subspace argmin is beyond the box;
                           --         isave(38) = the number of free variables in the current
                           --                         iteration;
                           --         isave(39) = the number of active constraints in the current
                           --                         iteration;
                           --         n + 1 - isave(40) = the number of variables leaving the set of
                           --                           active constraints in the current iteration;
                           --         isave(41) = the number of variables entering the set of active
                           --                         constraints in the current iteration.
            -> Ptr Double  --18   dsave is a double precision working array of dimension 29.
                           --       On exit with 'task' = NEW_X, the following information is
                           --                                                             available:
                           --         dsave(1) = current 'theta' in the BFGS matrix;
                           --         dsave(2) = f(x) in the previous iteration;
                           --         dsave(3) = factr*epsmch;
                           --         dsave(4) = 2-norm of the line search direction vector;
                           --         dsave(5) = the machine precision epsmch generated by the code;
                           --         dsave(7) = the accumulated time spent on searching for
                           --                                                         Cauchy points;
                           --         dsave(8) = the accumulated time spent on
                           --                                                 subspace minimization;
                           --         dsave(9) = the accumulated time spent on line search;
                           --         dsave(11) = the slope of the line search function at
                           --                                  the current point of line search;
                           --         dsave(12) = the maximum relative step length imposed in
                           --                                                           line search;
                           --         dsave(13) = the infinity norm of the projected gradient;
                           --         dsave(14) = the relative step length in the line search;
                           --         dsave(15) = the slope of the line search function at
                           --                                 the starting point of the line search;
                           --         dsave(16) = the square of the 2-norm of the line search
                           --                                                      direction vector.
            -> IO ()

