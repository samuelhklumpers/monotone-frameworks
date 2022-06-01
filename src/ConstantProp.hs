module ConstantProp where

import MonotoneFrameworks (BoundedSemiLattice)

import Data.Map.Merge.Strict qualified as MM
import Data.Map.Strict qualified as M
import Std (compose, intercalate)

-- | @Either Int Bool@
data ConstLat = CI Int | CB Bool deriving (Show, Eq)

-- | Newtype for maps with the semigroup of intersection (as opposed to left-biased union)
newtype ConstEnv = ConstEnv (M.Map String ConstLat) deriving Eq

-- | Lattice for the constant propagation analysis
type PtConstLat = Maybe ConstEnv

constEmpty :: ConstEnv
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

-- | Set this variable to a (non)-constant
setConst :: String -> Maybe ConstLat -> ConstEnv -> ConstEnv
setConst k v (ConstEnv e) = ConstEnv $ M.alter (const v) k e

-- | Mark this variable as non-constant
setNonConst :: String -> ConstEnv -> ConstEnv
setNonConst k = setConst k Nothing

-- | Lookup this variable in a @ConstEnv@
getConst ::  String -> ConstEnv -> Maybe ConstLat
getConst k (ConstEnv e) = M.lookup k e

-- | Set this variable to the value of the given expression, as a function from environment to value, evaluated in the given context
updateConst :: String -> (ConstEnv -> Maybe ConstLat) -> ConstEnv -> ConstEnv
updateConst k v e = setConst k (v e) e

-- | Transfer function for call: each parameter is initialized to the lattice value of its input expression
callConst :: [String] -> String -> [ConstEnv -> Maybe ConstLat] -> ConstEnv -> ConstEnv
callConst params resultNameProc values c = setNonConst resultNameProc $ compose fs c
  where
    fs :: [ConstEnv -> ConstEnv]
    fs = uncurry setConst <$> zip params (fmap ($ c) values)

-- | Transfer function for return: restore all values of the parameters and the return parameter, and copy the value from the return parameter to the context before the call.
retConst :: String -> [String] -> String -> ConstEnv -> ConstEnv -> ConstEnv
retConst resultNameCall inputs resultNameProc c r =
  setConst resultNameCall (getConst resultNameProc r) $
  compose
    ((\i -> setConst i (getConst i c)) <$> resultNameProc : inputs)
  r

-- * Functions lifting arithmetic/boolean operations to the lattice
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
