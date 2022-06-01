module ConstantBranch where

import ConstantProp (ConstEnv, ConstLat (CB), PtConstLat)
import MonotoneFrameworks (BoundedSemiLattice)

import Data.Set (Set, insert, member, delete)

-- Set Int records the dead labels
-- Nothing indicates unreachable
type ConstBranchLat = (PtConstLat, Set Int)

instance BoundedSemiLattice ConstBranchLat

ignoreDead :: Int -> (PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat
ignoreDead label f (lat, dead)
  | label `member` dead = (Nothing, dead)
  | otherwise           = (f lat, dead)

ignoreDead2 :: Int -> (PtConstLat -> PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat -> ConstBranchLat
ignoreDead2 label f (lat1, dead) (lat2, _) -- the callee can not reasonably mark labels outside its scope as dead
  | label `member` dead = (Nothing, dead)
  | otherwise           = (f lat1 lat2, dead)

constBranchId :: Int -> ConstBranchLat -> ConstBranchLat
constBranchId = flip ignoreDead id 

constBranchIf :: (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchIf cond t f (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB True)  -> (lat, insert f $ delete t dead)
    Just (CB False) -> (lat, insert t $ delete f dead)
    _               -> (lat, delete t $ delete f dead)

constBranchWhile :: Maybe Int -> (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchWhile succ cond label labelc (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB False) -> (lat, doIf delete succ $ insert label dead)
    Just (CB True)  -> (lat, doIf insert succ $ delete label dead)
    _               -> (lat, doIf delete succ $ delete label dead)
    -- we would like to mark the statement after the while block as dead when the condition is Just (CB True), but we can't (yet)

doIf :: (a -> b -> b) -> Maybe a -> b -> b
doIf _ Nothing  = id
doIf f (Just x) = f x