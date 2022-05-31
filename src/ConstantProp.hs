{-# LANGUAGE FlexibleInstances #-}

module ConstantProp where

import Data.Bifunctor
import Data.Set

import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as MM
import qualified Data.Maybe as Maybe

import Std (intercalate, join, compose)
import MonotoneFrameworks
import Analyses


data ConstLat = CI Int | CB Bool deriving (Show, Eq)
newtype ConstEnv = ConstEnv (M.Map String ConstLat) deriving Eq
type PtConstLat = Maybe ConstEnv

constEmpty = ConstEnv mempty

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

instance BoundedSemiLattice PtConstLat

setConst :: String -> Maybe ConstLat -> ConstEnv -> ConstEnv
setConst k v (ConstEnv e) = ConstEnv $ M.alter (const v) k e

setNonConst :: String -> ConstEnv -> ConstEnv
setNonConst k = setConst k Nothing

getConst ::  String -> ConstEnv -> Maybe ConstLat
getConst k (ConstEnv e) = M.lookup k e

updateConst :: String -> (ConstEnv -> Maybe ConstLat) -> ConstEnv -> ConstEnv
updateConst k v e = setConst k (v e) e

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

constMul :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constMul (Just (CI 0)) y = Just $ CI 0
constMul x (Just (CI 0)) = Just $ CI 0
constMul x y               = cIII (*) x y

cIIB :: (Int -> Int -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cIIB f x y = join $ cIIB' f <$> x <*> y

cBBB :: (Bool -> Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cBBB f x y = join $ cBBB' f <$> x <*> y

constAnd :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constAnd (Just (CB False)) y = Just $ CB False
constAnd x (Just (CB False)) = Just $ CB False
constAnd x y                 = cBBB (&&) x y

constOr :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constOr (Just (CB True)) y = Just $ CB True
constOr x (Just (CB True)) = Just $ CB True
constOr x y                 = cBBB (&&) x y

cBB :: (Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat
cBB f x = cBB' f =<< x
