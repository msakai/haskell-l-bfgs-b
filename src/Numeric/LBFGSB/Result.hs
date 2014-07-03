-- | The 'Result' data type encodes the minimization solution, as well
-- as auxiliary information about the minimization process.
module Numeric.LBFGSB.Result where

import qualified Data.Vector.Storable as V

-- | Stores the result of the minimization process.
data Result = Result {  
      solution :: V.Vector Double    -- ^ Solution point /if the minimization completed successfully/. See 'stopReason'.
    , backtrace :: [V.Vector Double] -- ^ The steps taken to reach the solution, in reverse order. Does not include the starting point.
    , stopReason :: StopReason       -- ^ The reason L-BFGS-B terminated. Only if this is
                                     -- 'Converged' should you consider the solution correct!
    }
              deriving (Show)

-- | The reason L-BFGS-B terminated.
data StopReason = 
      Converged    -- ^ The solution converged.
    | StepCount    -- ^ The number of steps exceeded the user's request.
    | Other String -- ^ Something else occured. In @'Other' s@, @s@ is the contents
                   -- of L-BFGS-B's @task@ variable on exit, as documented in the
                   -- source code of L-BFGS-B itself.
      deriving (Eq, Show)
