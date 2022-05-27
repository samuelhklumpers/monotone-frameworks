{-# LANGUAGE FlexibleInstances #-}


module Analyses where

import Data.Bifunctor
import Data.Set

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Maybe as Maybe

import Std (intercalate, join, compose)
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

