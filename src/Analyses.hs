{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
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


data ConstLat = CI Int | CB Bool | NonConst deriving (Show, Eq)
newtype PtConstLat = PtConstLat (M.Map String ConstLat) deriving Eq

instance Show PtConstLat where
    show (PtConstLat lat) = "[" ++ intercalate ", " vals ++ "]"
        where
            showCL (s, v) = s ++ " -> " ++ show v
            vals = showCL <$> M.toList lat


(!) :: PtConstLat -> String -> ConstLat
(PtConstLat m) ! k = Maybe.fromMaybe (error $ show k ++ " is not in " ++ show m) $ M.lookup k m

ptInsert :: String -> ConstLat -> PtConstLat -> PtConstLat
ptInsert k v (PtConstLat m) = PtConstLat (M.insert k v m)

ptLookup :: String -> PtConstLat -> Maybe ConstLat
ptLookup k (PtConstLat m) = M.lookup k m

ptLookupBot :: String -> PtConstLat -> ConstLat
ptLookupBot k (PtConstLat m) = M.findWithDefault NonConst k m

instance Semigroup ConstLat where
  CI x <> CI y = if x == y then CI x else NonConst
  CB x <> CB y = if x == y then CB x else NonConst
  _    <> _       = NonConst

instance Semigroup PtConstLat where
  (PtConstLat x) <> (PtConstLat y) = PtConstLat $ MM.merge MM.preserveMissing MM.preserveMissing (MM.zipWithMatched (const (<>))) x y

instance Monoid PtConstLat where
  mempty = PtConstLat mempty


instance Monoid ConstLat where
  mempty = NonConst

instance BoundedSemiLattice PtConstLat where



findProc :: String -> DStar -> Proc''
findProc = (Maybe.fromJust .) . lookup

surviveInto :: Set String -> String -> Set String -> Set String -> Set String
surviveInto env name vars dst = if name `member` env then alive <> vars else alive
  where
    alive = delete name dst

survive :: String -> Set String -> Set String -> Set String
survive name vars env = surviveInto env name vars env

surviveOne :: String -> String -> Set String -> Set String
surviveOne name var = survive name (singleton var)

constInto :: PtConstLat -> String -> (PtConstLat -> ConstLat) -> PtConstLat -> PtConstLat
constInto env name exp = ptInsert name (exp env)

updateConst :: String -> (PtConstLat -> ConstLat) -> PtConstLat -> PtConstLat
updateConst name exp env = constInto env name exp env

callStrong :: [String] -> [Set String] -> Set String -> Set String -> Set String
callStrong inputs params r c = appEndo (mconcat $ Endo <$> fs) r
 where
  fs :: [Set String -> Set String]
  fs = surviveInto c <$> inputs <*> params

retStrong :: String -> String -> Set String -> Set String
retStrong name var r = surviveInto r name (singleton var) mempty

callConst :: [String] -> String -> [PtConstLat -> ConstLat] -> PtConstLat -> PtConstLat
callConst inputs resultNameProc params c =
  ptInsert resultNameProc NonConst $
  appEndo (mconcat $ Endo <$> fs) $
  c
  where
    fs :: [PtConstLat -> PtConstLat]
    fs = uncurry updateConst <$> zip inputs params

retConst :: String -> [String] -> String -> PtConstLat -> PtConstLat -> PtConstLat
retConst resultNameCall inputs resultNameProc c r =
  ptInsert resultNameCall (ptLookupBot resultNameProc r) $
  compose
    ((\i -> ptInsert i (ptLookupBot i c)) <$> resultNameProc : inputs) $
  r

cIII :: (Int -> Int -> Int) -> ConstLat -> ConstLat -> ConstLat
cIII f (CI x) (CI y) = CI (x `f` y)
cIII _ _ _           = NonConst

cIIB :: (Int -> Int -> Bool) -> ConstLat -> ConstLat -> ConstLat
cIIB f (CI x) (CI y) = CB (x `f` y)
cIIB f _ _           = NonConst

cBBB :: (Bool -> Bool -> Bool) -> ConstLat -> ConstLat -> ConstLat
cBBB f (CB x) (CB y) = CB (x `f` y)
cBBB _ _ _           = NonConst

cBB :: (Bool -> Bool) -> ConstLat -> ConstLat
cBB f (CB x) = CB (f x)
cBB _ _      = NonConst
