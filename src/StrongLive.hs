module StrongLive where

import MonotoneFrameworks (BoundedSemiLattice)

import Data.Set (Set, delete, member, singleton)
import Std (compose)

instance BoundedSemiLattice (Set String)

moveAlive :: Set String -> String -> Set String -> Set String -> Set String
moveAlive src name freeVars dst = if name `member` src then dst' <> freeVars else dst'
  where
    dst' = delete name dst

keepAlive :: String -> Set String -> Set String -> Set String
keepAlive name freeVars alive = moveAlive alive name freeVars alive

aliveInCall :: [String] -> String -> [Set String] -> Set String -> Set String -> Set String
aliveInCall params returnVar freeVars aliveOutEntry = compose fs . delete returnVar
 where
  move :: String -> Set String -> Set String -> Set String
  move param freeVars alive = if param `member` aliveOutEntry then alive <> freeVars else alive

  fs :: [Set String -> Set String]
  fs = uncurry move <$> zip params freeVars

aliveInExit :: String -> String -> Set String -> Set String
aliveInExit returnVarOut returnVarIn aliveOutReturn = if returnVarOut `member` aliveOutReturn then singleton returnVarIn else mempty
