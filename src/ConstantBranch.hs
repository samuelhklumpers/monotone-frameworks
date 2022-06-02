module ConstantBranch where

import ConstantProp (ConstEnv, ConstLat (CB), PtConstLat)
import MonotoneFrameworks (BoundedSemiLattice)

import Control.Lens (coerced, under)
import Data.Set (Set, delete, insert, intersection, member)


-- | Newtype for sets with the intersection semigroup (as opposed to union).
newtype Intersect a = Intersect (Set a) deriving (Eq, Show)

instance Ord a => Semigroup (Intersect a) where
  Intersect a <> Intersect b = Intersect $ intersection a b

-- | Lattice for branch aware constant propagation
type ConstBranchLat = (PtConstLat, Maybe (Intersect Int))

instance BoundedSemiLattice ConstBranchLat

-- | Element relation for sets with an universal set adjoined: everything is in the universe (@Nothing@)
element :: Ord a => a -> Maybe (Intersect a) -> Bool
element _ Nothing              = True
element a (Just (Intersect x)) = a `member` x

-- | Ignore the unary transfer function at this label if it is dead
ignoreDead :: Int -> (PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat
ignoreDead label f (lat, dead)
  | label `element` dead = (Nothing, dead)
  | otherwise            = (f lat, dead)

-- | Ignore the binary transfer function at this label if it is dead
ignoreDead2 :: Int -> (PtConstLat -> PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat -> ConstBranchLat
ignoreDead2 label f (lat1, dead) (lat2, _) -- the callee can not reasonably mark labels outside its scope as dead
  | label `element` dead = (Nothing, dead)
  | otherwise            = (f lat1 lat2, dead)

-- | Discard the analysis value at this label if it is dead
constBranchId :: Int -> ConstBranchLat -> ConstBranchLat
constBranchId = flip ignoreDead id 

-- | Transfer function for @if@:
-- if the condition is always @true@, the @else@ branch is unreachable (dead) and the @then@ branch is made reachable,
-- and vice versa when the condition is always @false@.
-- Otherwise, both are reachable.
constBranchIf :: (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchIf cond t f (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB True)  -> (lat, under coerced (insert f . delete t) <$> dead)
    Just (CB False) -> (lat, under coerced (insert t . delete f) <$> dead)
    _               -> (lat, under coerced (delete t . delete f) <$> dead)

-- | Transfer function for @while@:
-- if the condition is always @false@, the body is dead, and the label after the loop is made reachable,
-- if it is always @true@, then the program loops, and the body is made reachable, and the label after is dead.
-- Otherwise, both are reachable.
constBranchWhile :: Maybe Int -> (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchWhile succ cond label labelc (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB False) -> (lat, under coerced (doIf delete succ . insert label) <$> dead)
    Just (CB True)  -> (lat, under coerced (doIf insert succ . delete label) <$> dead)
    _               -> (lat, under coerced (doIf delete succ . delete label) <$> dead)

-- | Apply a binary function with optional first argument, if @Nothing@, do nothing.
doIf :: (a -> b -> b) -> Maybe a -> b -> b
doIf _ Nothing  = id
doIf f (Just x) = f x