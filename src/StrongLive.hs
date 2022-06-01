module StrongLive where

import MonotoneFrameworks (BoundedSemiLattice)

import Data.Set (Set, insert, delete, union, member, singleton, difference, fromList)
import Std (compose)

instance BoundedSemiLattice (Set String)

-- | If @name@ is live in @src@, then the set @freeVars@ is live in @dst@
moveAlive :: Set String -> String -> Set String -> Set String -> Set String
moveAlive src name freeVars dst = if name `member` src then dst' <> freeVars else dst'
  where
    dst' = delete name dst

-- | If @name@ is live, then the set @freeVars@ is live
keepAlive :: String -> Set String -> Set String -> Set String
keepAlive name freeVars alive = moveAlive alive name freeVars alive

-- | Transfer function for call: everything that lives in the procedure remains live, except for the parameters,
-- everything that lives after the return remains live, except for the return parameter,
-- and for each parameter that is live in the procedure, all the free variables of its input value are live.
aliveInCall :: [String] -> String -> [Set String] -> Set String -> Set String -> Set String
aliveInCall params returnVar freeVars aliveOutCall aliveOutEntry = compose fs (delete returnVar aliveOutCall) `union` (aliveOutEntry `difference` fromList params)
 where
  move :: String -> Set String -> Set String -> Set String
  move param freeVars alive = if param `member` aliveOutEntry then alive <> freeVars else alive

  fs :: [Set String -> Set String]
  fs = uncurry move <$> zip params freeVars

-- | Transfer function for return: everything that lives after the return remains live, except for the output variable and input parameters,
-- if the output variable was live then the output parameter also becomes live
aliveInExit :: [String] -> String -> String -> Set String -> Set String
aliveInExit params returnVarOut returnVarIn aliveOutReturn = applyIf (insert returnVarIn) (returnVarOut `member` aliveOutReturn) (aliveOutReturn `difference` fromList params)

applyIf :: (a -> a) -> Bool -> a -> a
applyIf _ False = id
applyIf f True  = f