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

cIII :: (Int -> Int -> Int) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cIII f (Just (CI x)) (Just (CI y)) = Just $ CI (x `f` y)
cIII _ (Just _) (Just _) = error "type error"
cIII _ _ _ = Nothing

constMul :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constMul (Just (CI 0)) Nothing = Just $ CI 0
constMul Nothing (Just (CI 0)) = Just $ CI 0
constMul x y               = cIII (*) x y

cIIB :: (Int -> Int -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cIIB f (Just (CI x)) (Just (CI y)) = Just $ CB (x `f` y)
cIIB _ (Just _) (Just _) = error "type error"
cIIB _ _ _ = Nothing

cBBB :: (Bool -> Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
cBBB f (Just (CB x)) (Just (CB y)) = Just $ CB (x `f` y)
cBBB _ (Just _) (Just _) = error "type error"
cBBB _ _ _ = Nothing

constAnd :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constAnd (Just (CB False)) Nothing = Just $ CB False
constAnd Nothing (Just (CB False)) = Just $ CB False
constAnd x y                 = cBBB (&&) x y

constOr :: Maybe ConstLat -> Maybe ConstLat -> Maybe ConstLat
constOr (Just (CB True)) Nothing = Just $ CB True
constOr Nothing (Just (CB True)) = Just $ CB True
constOr x y                 = cBBB (&&) x y

cBB :: (Bool -> Bool) -> Maybe ConstLat -> Maybe ConstLat
cBB f (Just (CB x)) = Just $ CB (f x)
cBB f (Just _) = error "type error"
cBB f Nothing = Nothing
