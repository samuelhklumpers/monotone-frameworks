{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE FlexibleInstances #-}
module Analyses where


import Debug.Trace

import Data.Bifunctor
import Data.Set

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Maybe as Maybe

import Std (Endo(..), intercalate, join, compose)
import MonotoneFrameworks


data Dir = Forward | Backward deriving (Show, Eq)

-- * Transfer functions per label
type DifTrans m = (M.Map Int (m -> m), M.Map Int (m -> m -> m))


insertL :: Int -> (m -> m) -> DifTrans m -> DifTrans m
insertL = (first .) . M.insert

insertR :: Int -> (m -> m -> m) -> DifTrans m -> DifTrans m
insertR = (second .) . M.insert

singleL :: Int -> (m -> m) -> DifTrans m
singleL i x = insertL i x mempty

singleR :: Int -> (m -> m -> m) -> DifTrans m
singleR i x = insertR i x mempty

lookupL :: Int -> DifTrans p -> (p -> p)
lookupL i (m, _) = Maybe.fromMaybe (error $ show i ++ " is not in " ++ show (M.keys m)) $ M.lookup i m

lookupR :: Int -> DifTrans p -> (p -> p -> p)
lookupR i (_, m) = Maybe.fromMaybe (error $ show i ++ " is not in " ++ show (M.keys m)) $ M.lookup i m

data Proc'' = Proc'' { procEntry :: Int, procExit :: Int, procName :: String, procInp :: [String], procOut :: String } deriving (Show, Eq, Ord)
type DStar = [(String, Proc'')]

type Edge = (Int, Int)
data Inter = Inter Int Int Int Int deriving (Eq, Ord)

instance Show Inter where
  show (Inter a b c d) = show (a, b, c, d)

findProc :: String -> DStar -> Proc''
findProc = (Maybe.fromJust .) . lookup

data ConstLat = CI Int | CB Bool deriving (Show, Eq)
newtype ConstEnv = ConstEnv (M.Map String ConstLat) deriving Eq
type PtConstLat = Maybe ConstEnv

instance Show ConstEnv where
    show (ConstEnv lat) = "[" ++ intercalate ", " vals ++ "]"
        where
            showCL (s, v) = s ++ " -> " ++ show v
            vals = showCL <$> M.toList lat

instance Semigroup ConstEnv where
  (ConstEnv x) <> (ConstEnv y) = ConstEnv $ MM.merge MM.dropMissing MM.dropMissing (MM.zipWithMaybeMatched f) x y
    where
      f :: String -> ConstLat -> ConstLat -> Maybe ConstLat
      f _ a b = if a == b then Just a else Nothing

instance Monoid ConstEnv where
  mempty = ConstEnv mempty

instance BoundedSemiLattice ConstEnv

instance BoundedSemiLattice (Set String)

surviveInto :: Set String -> String -> Set String -> Set String -> Set String
surviveInto env name vars dst = if name `member` env then alive <> vars else alive
  where
    alive = delete name dst

survive :: String -> Set String -> Set String -> Set String
survive name vars env = surviveInto env name vars env

surviveOne :: String -> String -> Set String -> Set String
surviveOne name var = survive name (singleton var)


setConst :: String -> Maybe ConstLat -> ConstEnv -> ConstEnv
setConst k v (ConstEnv e) = ConstEnv $ M.alter (const v) k e

setNonConst :: String -> ConstEnv -> ConstEnv
setNonConst k = setConst k Nothing

getConst ::  String -> ConstEnv -> Maybe ConstLat
getConst k (ConstEnv e) = M.lookup k e

updateConst :: String -> (ConstEnv -> Maybe ConstLat) -> ConstEnv -> ConstEnv
updateConst k v e = setConst k (v e) e

callStrong :: [String] -> [Set String] -> Set String -> Set String -> Set String
callStrong inputs params r c = appEndo (mconcat $ Endo <$> fs) r
 where
  fs :: [Set String -> Set String]
  fs = surviveInto c <$> inputs <*> params

retStrong :: String -> String -> Set String -> Set String
retStrong name var r = surviveInto r name (singleton var) mempty

callConst :: [String] -> String -> [ConstEnv -> Maybe ConstLat] -> ConstEnv -> ConstEnv
callConst params resultNameProc values c = setNonConst resultNameProc $ compose fs c
  where
    fs :: [ConstEnv -> ConstEnv]
    fs = uncurry setConst <$> zip params (fmap ($ c) values)

retConst :: String -> [String] -> String -> ConstEnv -> ConstEnv -> ConstEnv
retConst resultNameCall inputs resultNameProc c r =
  setConst resultNameCall (getConst resultNameProc r) $
  compose
    ((\i -> setConst i (getConst i c)) <$> resultNameProc : inputs)
  r

cIII' :: (Int -> Int -> Int) -> ConstLat -> ConstLat -> Maybe ConstLat
cIII' f (CI x) (CI y) = Just $ CI (x `f` y)
cIII' _ _ _           = Nothing

cIIB' :: (Int -> Int -> Bool) -> ConstLat -> ConstLat -> Maybe ConstLat
cIIB' f (CI x) (CI y) = Just $ CB (x `f` y)
cIIB' f _ _           = Nothing

cBBB' :: (Bool -> Bool -> Bool) -> ConstLat -> ConstLat -> Maybe ConstLat
cBBB' f (CB x) (CB y) = Just $ CB (x `f` y)
cBBB' _ _ _           = Nothing

cBB' :: (Bool -> Bool) -> ConstLat -> Maybe ConstLat
cBB' f (CB x) = Just $ CB (f x)
cBB' _ _      = Nothing

cIII :: (Int -> Int -> Int) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cIII f x y = join $ cIII' f <$> x <*> y

cIIB :: (Int -> Int -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cIIB f x y = join $ cIIB' f <$> x <*> y

cBBB :: (Bool -> Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cBBB f x y = join $ cBBB' f <$> x <*> y

cBB :: (Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat
cBB f x = cBB' f =<< x
