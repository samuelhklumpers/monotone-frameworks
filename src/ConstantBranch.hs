module ConstantBranch where

import ConstantProp (ConstEnv, ConstLat (CB), PtConstLat)
import MonotoneFrameworks (BoundedSemiLattice)
import Control.Lens(under, coerced)

import Data.Set (Set, insert, intersection, member, delete)


newtype Intersect a = Intersect (Set a) deriving (Eq, Show)

instance Ord a => Semigroup (Intersect a) where
  Intersect a <> Intersect b = Intersect $ intersection a b

type ConstBranchLat = (PtConstLat, Maybe (Intersect Int))

instance BoundedSemiLattice ConstBranchLat

element :: Ord a => a -> Maybe (Intersect a) -> Bool
element _ Nothing              = True
element a (Just (Intersect x)) = a `member` x

ignoreDead :: Int -> (PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat
ignoreDead label f (lat, dead)
  | label `element` dead = (Nothing, dead)
  | otherwise            = (f lat, dead)

ignoreDead2 :: Int -> (PtConstLat -> PtConstLat -> PtConstLat) -> ConstBranchLat -> ConstBranchLat -> ConstBranchLat
ignoreDead2 label f (lat1, dead) (lat2, _) -- the callee can not reasonably mark labels outside its scope as dead
  | label `element` dead = (Nothing, dead)
  | otherwise            = (f lat1 lat2, dead)

constBranchId :: Int -> ConstBranchLat -> ConstBranchLat
constBranchId = flip ignoreDead id 

constBranchIf :: (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchIf cond t f (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB True)  -> (lat, under coerced (insert f . delete t) <$> dead)
    Just (CB False) -> (lat, under coerced (insert t . delete f) <$> dead)
    _               -> (lat, under coerced (delete t . delete f) <$> dead)

constBranchWhile :: Maybe Int -> (ConstEnv -> Maybe ConstLat) -> Int -> Int -> ConstBranchLat -> ConstBranchLat
constBranchWhile succ cond label labelc (lat, dead) = case lat of
  Nothing  -> (Nothing, dead)
  Just env -> case cond env of
    Just (CB False) -> (lat, under coerced (doIf delete succ . insert label) <$> dead)
    Just (CB True)  -> (lat, under coerced (doIf insert succ . delete label) <$> dead)
    _               -> (lat, under coerced (doIf delete succ . delete label) <$> dead)

doIf :: (a -> b -> b) -> Maybe a -> b -> b
doIf _ Nothing  = id
doIf f (Just x) = f x