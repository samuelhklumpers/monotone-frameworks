

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import qualified Data.Map.Merge.Strict as MM

import Data.Bifunctor

import Data.Set

import MonotoneFrameworks

import Std (Endo(..))

instance Semigroup Bool where
  False <> x = x
  x <> False = x

instance Monoid Bool where
  mappend = (<>)
  mempty = False

--                isBackward
type DifTrans m = (Bool, (M.Map Int (m -> m), M.Map Int (m -> m -> m)))


insertL :: Int -> (m -> m) -> DifTrans m -> DifTrans m
insertL i f (b, m) = (b, first (M.insert i f) m)

insertR :: Int -> (m -> m -> m) -> DifTrans m -> DifTrans m
insertR i f (b, m) = (b, second (M.insert i f) m)

singleL i x = insertL i x (False, mempty)
singleR i x = insertR i x (False, mempty)

lookupL :: Int -> DifTrans p -> (p -> p)
lookupL i (_, (m, _)) = Maybe.fromJust $ M.lookup i m

lookupR :: Int -> DifTrans p -> (p -> p -> p)
lookupR i (_, (_, m)) = Maybe.fromJust $ M.lookup i m

forwardAnalysis :: DifTrans m
forwardAnalysis  = (False, mempty)

backwardAnalysis :: DifTrans m
backwardAnalysis = (True, mempty)

data Proc'' = Proc'' { procEntry :: Int, procExit :: Int, procName :: String, procInp :: [String], procOut :: String } deriving (Show, Eq, Ord)
type DStar = [(String, Proc'')]

type Edge = (Int, Int)
data Inter = Inter Int Int Int Int deriving (Eq, Ord)

instance Show Inter where
  show (Inter a b c d) = show (a, b, c, d)
{-# LINE 62 "AttributeGrammar.hs" #-}

{-# LINE 242 "AttributeGrammar.ag" #-}

data ConstLat = CI Int | CB Bool | NonConst deriving (Show, Eq)
newtype PtConstLat = PtConstLat (M.Map String ConstLat) deriving (Show, Eq)

(!) :: PtConstLat -> String -> ConstLat
(PtConstLat m) ! k = Maybe.fromMaybe (error $ (show k) ++ " is not in " ++ (show m)) $ M.lookup k m

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
{-# LINE 97 "AttributeGrammar.hs" #-}

{-# LINE 287 "AttributeGrammar.ag" #-}

indent :: [String] -> [String]
indent = fmap ("  " ++)

showLabel :: Int -> String
showLabel label = "\ESC[93m" ++ reverse (go label) ++ "\ESC[0m"
  where
    go :: Int -> String
    go x
      | x < 0     = error "Negative label"
      | r == 0    = subscript !! m : ""
      | otherwise = subscript !! m : go r
      where
        (r, m) = x `divMod` 10
    subscript = "₀₁₂₃₄₅₆₇₈₉"

addSemicolon :: [String] -> [String]
addSemicolon [] = []
addSemicolon xs = init xs ++ [last xs ++ ";"]

findProc :: String -> DStar -> Proc''
findProc = (Maybe.fromJust .) . lookup 

surviveInto :: Set String -> String -> Set String -> Set String -> Set String
surviveInto env name vars dst = if name `member` env then alive <> vars else alive
  where
    alive = delete name dst

survive :: String -> Set String -> Set String -> Set String
survive name vars env = surviveInto env name vars env

surviveOne :: String -> String -> Set String -> Set String
surviveOne name var env = survive name (singleton var) env

constInto :: PtConstLat -> String -> (PtConstLat -> ConstLat) -> PtConstLat -> PtConstLat
constInto env name exp dst = ptInsert name val' dst
  where
    val  = exp env
    val' = case ptLookup name env of
      Just y  -> if val == y then y else NonConst
      Nothing -> val

updateConst :: String -> (PtConstLat -> ConstLat) -> PtConstLat -> PtConstLat
updateConst name exp env = constInto env name exp env
{-# LINE 144 "AttributeGrammar.hs" #-}

{-# LINE 333 "AttributeGrammar.ag" #-}

callStrong :: [String] -> [Set String] -> Set String -> Set String -> Set String
callStrong inputs params r c = appEndo (mconcat $ Endo <$> fs) r
 where
  fs :: [Set String -> Set String]
  fs = surviveInto c <$> inputs <*> params

retStrong :: String -> String -> Set String -> Set String
retStrong name var r = surviveInto r name (singleton var) mempty

callConst :: [String] -> [PtConstLat -> ConstLat] -> PtConstLat -> PtConstLat
callConst inputs params c = appEndo (mconcat $ Endo <$> fs) mempty
  where
    fs :: [PtConstLat -> PtConstLat]
    fs = updateConst <$> inputs <*> params

retConst :: String -> String -> PtConstLat -> PtConstLat -> PtConstLat
retConst retName outName c r = ptInsert outName (ptLookupBot retName c) r
{-# LINE 165 "AttributeGrammar.hs" #-}

{-# LINE 520 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"

cIII :: (Int -> Int -> Int) -> ConstLat -> ConstLat -> ConstLat
cIII f (CI x) (CI y) = CI (x `f` y)
cIII _ NonConst _    = NonConst
cIII _ _ NonConst    = NonConst

cIIB :: (Int -> Int -> Bool) -> ConstLat -> ConstLat -> ConstLat
cIIB f (CI x) (CI y) = CB (x `f` y)
cIIB _ NonConst _    = NonConst
cIIB _ _ NonConst    = NonConst

cBBB :: (Bool -> Bool -> Bool) -> ConstLat -> ConstLat -> ConstLat
cBBB f (CB x) (CB y) = CB (x `f` y)
cBBB _ NonConst _    = NonConst
cBBB _ _ NonConst    = NonConst

cBB :: (Bool -> Bool) -> ConstLat -> ConstLat
cBB f (CB x)   = CB (f x)
cBB _ NonConst = NonConst
{-# LINE 191 "AttributeGrammar.hs" #-}
-- BExpr -------------------------------------------------------
data BExpr = BConst (Bool)
           | BVar (String)
           | LessThan (IExpr) (IExpr)
           | GreaterThan (IExpr) (IExpr)
           | LessEqual (IExpr) (IExpr)
           | GreaterEqual (IExpr) (IExpr)
           | IEqual (IExpr) (IExpr)
           | BEqual (BExpr) (BExpr)
           | And (BExpr) (BExpr)
           | Or (BExpr) (BExpr)
           | Not (BExpr)
           deriving ( Eq,Show)
-- cata
sem_BExpr :: BExpr ->
             T_BExpr
sem_BExpr (BConst _val) =
    (sem_BExpr_BConst _val)
sem_BExpr (BVar _name) =
    (sem_BExpr_BVar _name)
sem_BExpr (LessThan _left _right) =
    (sem_BExpr_LessThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterThan _left _right) =
    (sem_BExpr_GreaterThan (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (LessEqual _left _right) =
    (sem_BExpr_LessEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (GreaterEqual _left _right) =
    (sem_BExpr_GreaterEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (IEqual _left _right) =
    (sem_BExpr_IEqual (sem_IExpr _left) (sem_IExpr _right))
sem_BExpr (BEqual _left _right) =
    (sem_BExpr_BEqual (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (And _left _right) =
    (sem_BExpr_And (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Or _left _right) =
    (sem_BExpr_Or (sem_BExpr _left) (sem_BExpr _right))
sem_BExpr (Not _val) =
    (sem_BExpr_Not (sem_BExpr _val))
-- semantic domain
type T_BExpr = ( ( PtConstLat -> ConstLat ),( Set String ),Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {expValSpace_Syn_BExpr :: ( PtConstLat -> ConstLat ),freeVars_Syn_BExpr :: ( Set String ),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOexpValSpace _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 579 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 251 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 580 "AttributeGrammar.ag" #-}
              10
              {-# LINE 256 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 581 "AttributeGrammar.ag" #-}
              \_ -> CB val_
              {-# LINE 261 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 266 "AttributeGrammar.hs" #-}
              )
         _self =
             BConst val_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 583 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 284 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 584 "AttributeGrammar.ag" #-}
              10
              {-# LINE 289 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 585 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 294 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 586 "AttributeGrammar.ag" #-}
              ptLookupBot name_
              {-# LINE 299 "AttributeGrammar.hs" #-}
              )
         _self =
             BVar name_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 588 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 328 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 589 "AttributeGrammar.ag" #-}
              4
              {-# LINE 333 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 590 "AttributeGrammar.ag" #-}
              \env -> cIIB (<) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 338 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 343 "AttributeGrammar.hs" #-}
              )
         _self =
             LessThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 592 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 376 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 593 "AttributeGrammar.ag" #-}
              4
              {-# LINE 381 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 594 "AttributeGrammar.ag" #-}
              \env -> cIIB (>) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 386 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 391 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 596 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 424 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 597 "AttributeGrammar.ag" #-}
              4
              {-# LINE 429 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 598 "AttributeGrammar.ag" #-}
              \env -> cIIB (<=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 434 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 439 "AttributeGrammar.hs" #-}
              )
         _self =
             LessEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 600 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 472 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 601 "AttributeGrammar.ag" #-}
              4
              {-# LINE 477 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 602 "AttributeGrammar.ag" #-}
              \env -> cIIB (>=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 482 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 487 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 604 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 520 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 605 "AttributeGrammar.ag" #-}
              4
              {-# LINE 525 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 606 "AttributeGrammar.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 530 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 535 "AttributeGrammar.hs" #-}
              )
         _self =
             IEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 608 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 568 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 609 "AttributeGrammar.ag" #-}
              4
              {-# LINE 573 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 610 "AttributeGrammar.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 578 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 583 "AttributeGrammar.hs" #-}
              )
         _self =
             BEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 612 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 616 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 613 "AttributeGrammar.ag" #-}
              3
              {-# LINE 621 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 614 "AttributeGrammar.ag" #-}
              \env -> cBBB (&&) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 626 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 631 "AttributeGrammar.hs" #-}
              )
         _self =
             And _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 616 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 664 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 617 "AttributeGrammar.ag" #-}
              2
              {-# LINE 669 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 618 "AttributeGrammar.ag" #-}
              \env -> cBBB (||) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 674 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 679 "AttributeGrammar.hs" #-}
              )
         _self =
             Or _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _valIexpValSpace :: ( PtConstLat -> ConstLat )
         _valIfreeVars :: ( Set String )
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 620 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 706 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 621 "AttributeGrammar.ag" #-}
              10
              {-# LINE 711 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 622 "AttributeGrammar.ag" #-}
              \env -> cBB not (_valIexpValSpace env)
              {-# LINE 716 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 721 "AttributeGrammar.hs" #-}
              )
         _self =
             Not _valIself
         _lhsOself =
             _self
         ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
-- Expr --------------------------------------------------------
data Expr = B (BExpr)
          | I (IExpr)
          deriving ( Eq,Show)
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (B _expr) =
    (sem_Expr_B (sem_BExpr _expr))
sem_Expr (I _expr) =
    (sem_Expr_I (sem_IExpr _expr))
-- semantic domain
type T_Expr = ( ( PtConstLat -> ConstLat ),( Set String ),String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {expValSpace_Syn_Expr :: ( PtConstLat -> ConstLat ),freeVars_Syn_Expr :: ( Set String ),pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Expr _lhsOexpValSpace _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _exprIexpValSpace :: ( PtConstLat -> ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 626 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 766 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 281 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 771 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 281 "AttributeGrammar.ag" #-}
              _exprIexpValSpace
              {-# LINE 780 "AttributeGrammar.hs" #-}
              )
         ( _exprIexpValSpace,_exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _exprIexpValSpace :: ( PtConstLat -> ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 628 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 800 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 281 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 805 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 281 "AttributeGrammar.ag" #-}
              _exprIexpValSpace
              {-# LINE 814 "AttributeGrammar.hs" #-}
              )
         ( _exprIexpValSpace,_exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( ( [PtConstLat -> ConstLat] ),( [Set String] ),String,Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {expValSpace_Syn_Exprs :: ( [PtConstLat -> ConstLat] ),freeVars_Syn_Exprs :: ( [Set String] ),pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Exprs _lhsOexpValSpace _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _lhsOexpValSpace :: ( [PtConstLat -> ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _hdIexpValSpace :: ( PtConstLat -> ConstLat )
         _hdIfreeVars :: ( Set String )
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIexpValSpace :: ( [PtConstLat -> ConstLat] )
         _tlIfreeVars :: ( [Set String] )
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 634 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 855 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 282 "AttributeGrammar.ag" #-}
              _hdIexpValSpace : _tlIexpValSpace
              {-# LINE 860 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 282 "AttributeGrammar.ag" #-}
              _hdIfreeVars : _tlIfreeVars
              {-# LINE 865 "AttributeGrammar.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIexpValSpace,_hdIfreeVars,_hdIpretty,_hdIself) =
             hd_
         ( _tlIexpValSpace,_tlIfreeVars,_tlIpretty,_tlIself) =
             tl_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOexpValSpace :: ( [PtConstLat -> ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 632 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 885 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 282 "AttributeGrammar.ag" #-}
              []
              {-# LINE 890 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 282 "AttributeGrammar.ag" #-}
              []
              {-# LINE 895 "AttributeGrammar.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOpretty,_lhsOself))
-- IExpr -------------------------------------------------------
data IExpr = IConst (Int)
           | Var (String)
           | Plus (IExpr) (IExpr)
           | Minus (IExpr) (IExpr)
           | Times (IExpr) (IExpr)
           | Divide (IExpr) (IExpr)
           | Deref (IExpr)
           deriving ( Eq,Show)
-- cata
sem_IExpr :: IExpr ->
             T_IExpr
sem_IExpr (IConst _val) =
    (sem_IExpr_IConst _val)
sem_IExpr (Var _name) =
    (sem_IExpr_Var _name)
sem_IExpr (Plus _left _right) =
    (sem_IExpr_Plus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Minus _left _right) =
    (sem_IExpr_Minus (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Times _left _right) =
    (sem_IExpr_Times (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Divide _left _right) =
    (sem_IExpr_Divide (sem_IExpr _left) (sem_IExpr _right))
sem_IExpr (Deref _ptr) =
    (sem_IExpr_Deref (sem_IExpr _ptr))
-- semantic domain
type T_IExpr = ( ( PtConstLat -> ConstLat ),( Set String ),Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {expValSpace_Syn_IExpr :: ( PtConstLat -> ConstLat ),freeVars_Syn_IExpr :: ( Set String ),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOexpValSpace _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 547 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 949 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 548 "AttributeGrammar.ag" #-}
              10
              {-# LINE 954 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 549 "AttributeGrammar.ag" #-}
              \_ -> CI val_
              {-# LINE 959 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 964 "AttributeGrammar.hs" #-}
              )
         _self =
             IConst val_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 551 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 982 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 552 "AttributeGrammar.ag" #-}
              10
              {-# LINE 987 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 553 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 992 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 554 "AttributeGrammar.ag" #-}
              ptLookupBot name_
              {-# LINE 997 "AttributeGrammar.hs" #-}
              )
         _self =
             Var name_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 556 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 1026 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 557 "AttributeGrammar.ag" #-}
              6
              {-# LINE 1031 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 558 "AttributeGrammar.ag" #-}
              \env -> cIII (+) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1036 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1041 "AttributeGrammar.hs" #-}
              )
         _self =
             Plus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 560 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 1074 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 561 "AttributeGrammar.ag" #-}
              6
              {-# LINE 1079 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 562 "AttributeGrammar.ag" #-}
              \env -> cIII (-) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1084 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1089 "AttributeGrammar.hs" #-}
              )
         _self =
             Minus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 564 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1122 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 565 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1127 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 566 "AttributeGrammar.ag" #-}
              \env -> cIII (*) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1132 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1137 "AttributeGrammar.hs" #-}
              )
         _self =
             Times _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( PtConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( PtConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 568 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1170 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 569 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1175 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 570 "AttributeGrammar.ag" #-}
              \env -> cIII div (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1180 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1185 "AttributeGrammar.hs" #-}
              )
         _self =
             Divide _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIexpValSpace,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIexpValSpace,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOexpValSpace :: ( PtConstLat -> ConstLat )
         _ptrIexpValSpace :: ( PtConstLat -> ConstLat )
         _ptrIfreeVars :: ( Set String )
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 572 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1212 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 573 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1217 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1222 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 280 "AttributeGrammar.ag" #-}
              _ptrIexpValSpace
              {-# LINE 1231 "AttributeGrammar.hs" #-}
              )
         ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
-- Proc --------------------------------------------------------
data Proc = Proc (String) (([String])) (String) (Stat)
          deriving ( Show)
-- cata
sem_Proc :: Proc ->
            T_Proc
sem_Proc (Proc _name _inp _out _stat) =
    (sem_Proc_Proc _name _inp _out (sem_Stat _stat))
-- semantic domain
type T_Proc = Int ->
              ( ( (String, Proc'') ),Int,Proc',Proc)
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {dStar_Syn_Proc :: ( (String, Proc'') ),label_Syn_Proc :: Int,labelled_Syn_Proc :: Proc',self_Syn_Proc :: Proc}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Proc _lhsOdStar _lhsOlabel _lhsOlabelled _lhsOself))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabelled :: Proc'
              _lhsOlabel :: Int
              _lhsOdStar :: ( (String, Proc'') )
              _lhsOself :: Proc
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 184 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1273 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 185 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1278 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1283 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 187 "AttributeGrammar.ag" #-}
                   (name_, Proc'' _lhsIlabel _statIlabel name_ inp_ out_)
                   {-# LINE 1288 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelExit _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelExit _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = DStar ->
               ( ( Set Int ),( Set Edge ),Int,( Set Inter ),( [String] ),Proc',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: DStar}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set Edge ),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ( Set Inter ),pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc',strongLive_Syn_Proc' :: ( DifTrans (Set String) ),valSpace_Syn_Proc' :: ( DifTrans PtConstLat )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelExit_ name_ inp_ out_ stat_ =
    (\ _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _statOdStar :: DStar
              _statOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Proc'
              _statIbreakLabels :: ( Set Int )
              _statIcontinueLabel :: ( Maybe Int )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set Edge )
              _statIinit :: Int
              _statIinterflow :: ( Set Inter )
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( DifTrans (Set String) )
              _statIvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty =
                  ({-# LINE 376 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1352 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 379 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1357 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 380 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1362 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 381 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1367 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 382 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1372 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 383 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1377 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 384 "AttributeGrammar.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIstrongLive
                   {-# LINE 1382 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 385 "AttributeGrammar.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIvalSpace
                   {-# LINE 1387 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1392 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Int ->
               ( DStar,Int,Procs',Procs)
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {dStar_Syn_Procs :: DStar,label_Syn_Procs :: Int,labelled_Syn_Procs :: Procs',self_Syn_Procs :: Procs}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel) =
    (let ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Procs _lhsOdStar _lhsOlabel _lhsOlabelled _lhsOself))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOdStar :: DStar
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIdStar :: ( (String, Proc'') )
              _hdIlabel :: Int
              _hdIlabelled :: Proc'
              _hdIself :: Proc
              _tlIdStar :: DStar
              _tlIlabel :: Int
              _tlIlabelled :: Procs'
              _tlIself :: Procs
              _lhsOlabelled =
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1441 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   _hdIdStar : _tlIdStar
                   {-# LINE 1446 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1455 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1460 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1465 "AttributeGrammar.hs" #-}
                   )
              ( _hdIdStar,_hdIlabel,_hdIlabelled,_hdIself) =
                  hd_ _hdOlabel
              ( _tlIdStar,_tlIlabel,_tlIlabelled,_tlIself) =
                  tl_ _tlOlabel
          in  ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOdStar :: DStar
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _lhsOlabelled =
                  ({-# LINE 176 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1482 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1487 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1496 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Procs' ------------------------------------------------------
type Procs' = [Proc']
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' list =
    (Prelude.foldr sem_Procs'_Cons sem_Procs'_Nil (Prelude.map sem_Proc' list))
-- semantic domain
type T_Procs' = DStar ->
                ( ( Set Edge ),( Set Inter ),( [String] ),Procs',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: DStar}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ( Set Edge ),interflow_Syn_Procs' :: ( Set Inter ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs',strongLive_Syn_Procs' :: ( DifTrans (Set String) ),valSpace_Syn_Procs' :: ( DifTrans PtConstLat )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar) =
    (let ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar
     in  (Syn_Procs' _lhsOflow _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOself :: Procs'
              _hdOdStar :: DStar
              _tlOdStar :: DStar
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set Edge )
              _hdIinit :: Int
              _hdIinterflow :: ( Set Inter )
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _hdIstrongLive :: ( DifTrans (Set String) )
              _hdIvalSpace :: ( DifTrans PtConstLat )
              _tlIflow :: ( Set Edge )
              _tlIinterflow :: ( Set Inter )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _tlIstrongLive :: ( DifTrans (Set String) )
              _tlIvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1547 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1552 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   _hdIinterflow <> _tlIinterflow
                   {-# LINE 1557 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1562 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   _hdIvalSpace <> _tlIvalSpace
                   {-# LINE 1567 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1576 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1581 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIpretty,_hdIself,_hdIstrongLive,_hdIvalSpace) =
                  hd_ _hdOdStar
              ( _tlIflow,_tlIinterflow,_tlIpretty,_tlIself,_tlIstrongLive,_tlIvalSpace) =
                  tl_ _tlOdStar
          in  ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOself :: Procs'
              _lhsOpretty =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1600 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1605 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 1610 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   backwardAnalysis
                   {-# LINE 1615 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   forwardAnalysis
                   {-# LINE 1620 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
-- Program -----------------------------------------------------
data Program = Program (Procs) (Stat)
             deriving ( Show)
-- cata
sem_Program :: Program ->
               T_Program
sem_Program (Program _procs _stat) =
    (sem_Program_Program (sem_Procs _procs) (sem_Stat _stat))
-- semantic domain
type T_Program = ( Program',Program)
data Inh_Program = Inh_Program {}
data Syn_Program = Syn_Program {labelled_Syn_Program :: Program',self_Syn_Program :: Program}
wrap_Program :: T_Program ->
                Inh_Program ->
                Syn_Program
wrap_Program sem (Inh_Program) =
    (let ( _lhsOlabelled,_lhsOself) = sem
     in  (Syn_Program _lhsOlabelled _lhsOself))
sem_Program_Program :: T_Procs ->
                       T_Stat ->
                       T_Program
sem_Program_Program procs_ stat_ =
    (let _procsOlabel :: Int
         _statOlabel :: Int
         _lhsOlabelled :: Program'
         _lhsOself :: Program
         _procsIdStar :: DStar
         _procsIlabel :: Int
         _procsIlabelled :: Procs'
         _procsIself :: Procs
         _statIlabel :: Int
         _statIlabelled :: Stat'
         _statIself :: Stat
         _procsOlabel =
             ({-# LINE 170 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1663 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 171 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1668 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 172 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1673 "AttributeGrammar.hs" #-}
              )
         _self =
             Program _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIdStar,_procsIlabel,_procsIlabelled,_procsIself) =
             procs_ _procsOlabel
         ( _statIlabel,_statIlabelled,_statIself) =
             stat_ _statOlabel
     in  ( _lhsOlabelled,_lhsOself))
-- Program' ----------------------------------------------------
data Program' = Program' (Procs') (Stat') (DStar)
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat _dStar) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat) _dStar)
-- semantic domain
type T_Program' = ( ( Set Int ),( Set Edge ),Int,( Set Inter ),String,Program',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set Edge ),init_Syn_Program' :: Int,interflow_Syn_Program' :: ( Set Inter ),pretty_Syn_Program' :: String,self_Syn_Program' :: Program',strongLive_Syn_Program' :: ( DifTrans (Set String) ),valSpace_Syn_Program' :: ( DifTrans PtConstLat )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         DStar ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _lhsOflow :: ( Set Edge )
         _lhsOstrongLive :: ( DifTrans (Set String) )
         _lhsOvalSpace :: ( DifTrans PtConstLat )
         _statOdStar :: DStar
         _statOcontinueLabel :: ( Maybe Int )
         _procsOdStar :: DStar
         _lhsOself :: Program'
         _lhsOinterflow :: ( Set Inter )
         _procsIflow :: ( Set Edge )
         _procsIinterflow :: ( Set Inter )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _procsIstrongLive :: ( DifTrans (Set String) )
         _procsIvalSpace :: ( DifTrans PtConstLat )
         _statIbreakLabels :: ( Set Int )
         _statIcontinueLabel :: ( Maybe Int )
         _statIfinal :: ( Set Int )
         _statIflow :: ( Set Edge )
         _statIinit :: Int
         _statIinterflow :: ( Set Inter )
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIstrongLive :: ( DifTrans (Set String) )
         _statIvalSpace :: ( DifTrans PtConstLat )
         _lhsOpretty =
             ({-# LINE 356 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1739 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 357 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1744 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 358 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1749 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 359 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1754 "AttributeGrammar.hs" #-}
              )
         _lhsOstrongLive =
             ({-# LINE 360 "AttributeGrammar.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1759 "AttributeGrammar.hs" #-}
              )
         _lhsOvalSpace =
             ({-# LINE 361 "AttributeGrammar.ag" #-}
              _procsIvalSpace <> _statIvalSpace
              {-# LINE 1764 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 362 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1769 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 363 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1774 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 364 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1779 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         _lhsOinterflow =
             ({-# LINE 277 "AttributeGrammar.ag" #-}
              _statIinterflow
              {-# LINE 1788 "AttributeGrammar.hs" #-}
              )
         ( _procsIflow,_procsIinterflow,_procsIpretty,_procsIself,_procsIstrongLive,_procsIvalSpace) =
             procs_ _procsOdStar
         ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
             stat_ _statOcontinueLabel _statOdStar
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace))
-- Stat --------------------------------------------------------
data Stat = Skip
          | IfThenElse (BExpr) (Stat) (Stat)
          | While (BExpr) (Stat)
          | Call (String) (Exprs) (String)
          | IAssign (String) (IExpr)
          | BAssign (String) (BExpr)
          | Seq (Stat) (Stat)
          | Malloc (String) (IExpr)
          | Free (IExpr)
          | RefAssign (IExpr) (IExpr)
          | Continue
          | Break
          deriving ( Show)
-- cata
sem_Stat :: Stat ->
            T_Stat
sem_Stat (Skip) =
    (sem_Stat_Skip)
sem_Stat (IfThenElse _cond _stat1 _stat2) =
    (sem_Stat_IfThenElse _cond (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (While _cond _stat) =
    (sem_Stat_While _cond (sem_Stat _stat))
sem_Stat (Call _name _params _out) =
    (sem_Stat_Call _name _params _out)
sem_Stat (IAssign _name _val) =
    (sem_Stat_IAssign _name _val)
sem_Stat (BAssign _name _val) =
    (sem_Stat_BAssign _name _val)
sem_Stat (Seq _stat1 _stat2) =
    (sem_Stat_Seq (sem_Stat _stat1) (sem_Stat _stat2))
sem_Stat (Malloc _name _size) =
    (sem_Stat_Malloc _name _size)
sem_Stat (Free _ptr) =
    (sem_Stat_Free _ptr)
sem_Stat (RefAssign _ptr _val) =
    (sem_Stat_RefAssign _ptr _val)
sem_Stat (Continue) =
    (sem_Stat_Continue)
sem_Stat (Break) =
    (sem_Stat_Break)
-- semantic domain
type T_Stat = Int ->
              ( Int,Stat',Stat)
data Inh_Stat = Inh_Stat {label_Inh_Stat :: Int}
data Syn_Stat = Syn_Stat {label_Syn_Stat :: Int,labelled_Syn_Stat :: Stat',self_Syn_Stat :: Stat}
wrap_Stat :: T_Stat ->
             Inh_Stat ->
             Syn_Stat
wrap_Stat sem (Inh_Stat _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Stat _lhsOlabel _lhsOlabelled _lhsOself))
sem_Stat_Skip :: T_Stat
sem_Stat_Skip =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1856 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1861 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_IfThenElse :: BExpr ->
                       T_Stat ->
                       T_Stat ->
                       T_Stat
sem_Stat_IfThenElse cond_ stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat1Iself :: Stat
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat2Iself :: Stat
              _stat1Olabel =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1888 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 196 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1893 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 197 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1898 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1903 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse cond_ _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              ( _stat1Ilabel,_stat1Ilabelled,_stat1Iself) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled,_stat2Iself) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_While :: BExpr ->
                  T_Stat ->
                  T_Stat
sem_Stat_While cond_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 201 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1929 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 202 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1934 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1939 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While cond_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Call :: String ->
                 Exprs ->
                 String ->
                 T_Stat
sem_Stat_Call name_ params_ out_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1960 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 207 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1965 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call name_ params_ out_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_IAssign :: String ->
                    IExpr ->
                    T_Stat
sem_Stat_IAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1983 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 211 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1988 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign name_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_BAssign :: String ->
                    BExpr ->
                    T_Stat
sem_Stat_BAssign name_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 214 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2006 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 215 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 2011 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign name_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Seq :: T_Stat ->
                T_Stat ->
                T_Stat
sem_Stat_Seq stat1_ stat2_ =
    (\ _lhsIlabel ->
         (let _stat1Olabel :: Int
              _stat2Olabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel :: Int
              _stat1Ilabel :: Int
              _stat1Ilabelled :: Stat'
              _stat1Iself :: Stat
              _stat2Ilabel :: Int
              _stat2Ilabelled :: Stat'
              _stat2Iself :: Stat
              _stat1Olabel =
                  ({-# LINE 218 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 2037 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 219 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 2042 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 220 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 2047 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 2056 "AttributeGrammar.hs" #-}
                   )
              ( _stat1Ilabel,_stat1Ilabelled,_stat1Iself) =
                  stat1_ _stat1Olabel
              ( _stat2Ilabel,_stat2Ilabelled,_stat2Iself) =
                  stat2_ _stat2Olabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Malloc :: String ->
                   IExpr ->
                   T_Stat
sem_Stat_Malloc name_ size_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 223 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2074 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 224 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 2079 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc name_ size_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Free :: IExpr ->
                 T_Stat
sem_Stat_Free ptr_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 227 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2096 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 228 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2101 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free ptr_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_RefAssign :: IExpr ->
                      IExpr ->
                      T_Stat
sem_Stat_RefAssign ptr_ val_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 231 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2119 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 232 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2124 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign ptr_ val_
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Continue :: T_Stat
sem_Stat_Continue =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 235 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2140 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 236 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2145 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Break :: T_Stat
sem_Stat_Break =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 239 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2161 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 240 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2166 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break
              _lhsOself =
                  _self
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Stat' -------------------------------------------------------
data Stat' = Skip' (Int)
           | IfThenElse' (Int) (BExpr) (Stat') (Stat')
           | While' (Int) (BExpr) (Stat')
           | Call' (Int) (Int) (String) (Exprs) (String)
           | IAssign' (Int) (String) (IExpr)
           | BAssign' (Int) (String) (BExpr)
           | Seq' (Stat') (Stat')
           | Malloc' (Int) (String) (IExpr)
           | Free' (Int) (IExpr)
           | RefAssign' (Int) (IExpr) (IExpr)
           | Continue' (Int)
           | Break' (Int)
           deriving ( Show)
-- cata
sem_Stat' :: (Stat') ->
             (T_Stat')
sem_Stat' (Skip' _label) =
    (sem_Stat'_Skip' _label)
sem_Stat' (IfThenElse' _labelc _cond _stat1 _stat2) =
    (sem_Stat'_IfThenElse' _labelc (sem_BExpr _cond) (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (While' _labelc _cond _stat) =
    (sem_Stat'_While' _labelc (sem_BExpr _cond) (sem_Stat' _stat))
sem_Stat' (Call' _labelCall _labelReturn _name _params _out) =
    (sem_Stat'_Call' _labelCall _labelReturn _name (sem_Exprs _params) _out)
sem_Stat' (IAssign' _label _name _val) =
    (sem_Stat'_IAssign' _label _name (sem_IExpr _val))
sem_Stat' (BAssign' _label _name _val) =
    (sem_Stat'_BAssign' _label _name (sem_BExpr _val))
sem_Stat' (Seq' _stat1 _stat2) =
    (sem_Stat'_Seq' (sem_Stat' _stat1) (sem_Stat' _stat2))
sem_Stat' (Malloc' _label _name _size) =
    (sem_Stat'_Malloc' _label _name (sem_IExpr _size))
sem_Stat' (Free' _label _ptr) =
    (sem_Stat'_Free' _label (sem_IExpr _ptr))
sem_Stat' (RefAssign' _label _ptr _val) =
    (sem_Stat'_RefAssign' _label (sem_IExpr _ptr) (sem_IExpr _val))
sem_Stat' (Continue' _label) =
    (sem_Stat'_Continue' _label)
sem_Stat' (Break' _label) =
    (sem_Stat'_Break' _label)
-- semantic domain
type T_Stat' = ( Maybe Int ) ->
               DStar ->
               ( ( Set Int ),( Maybe Int ),( Set Int ),( Set Edge ),Int,( Set Inter ),Bool,Bool,( [String] ),Stat',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: DStar}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),continueLabel_Syn_Stat' :: ( Maybe Int ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set Edge ),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ( Set Inter ),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( DifTrans (Set String) ),valSpace_Syn_Stat' :: ( DifTrans PtConstLat )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar) =
    (let ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIcontinueLabel _lhsIdStar
     in  (Syn_Stat' _lhsObreakLabels _lhsOcontinueLabel _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 389 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2246 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 390 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2251 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 391 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2256 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 392 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2261 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 393 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2266 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 394 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2271 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2276 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2281 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   backwardAnalysis
                   {-# LINE 2286 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   forwardAnalysis
                   {-# LINE 2291 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2300 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _condIexpValSpace :: ( PtConstLat -> ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1IbreakLabels :: ( Set Int )
              _stat1IcontinueLabel :: ( Maybe Int )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set Edge )
              _stat1Iinit :: Int
              _stat1Iinterflow :: ( Set Inter )
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( DifTrans (Set String) )
              _stat1IvalSpace :: ( DifTrans PtConstLat )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IcontinueLabel :: ( Maybe Int )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set Edge )
              _stat2Iinit :: Int
              _stat2Iinterflow :: ( Set Inter )
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( DifTrans (Set String) )
              _stat2IvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty =
                  ({-# LINE 397 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2365 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 404 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2370 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 405 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2375 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 406 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2380 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 407 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2385 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 408 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2390 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 409 "AttributeGrammar.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2395 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 410 "AttributeGrammar.ag" #-}
                   insertL labelc_ id $ _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2400 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2405 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2410 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2419 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2424 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2429 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2434 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2439 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _statOcontinueLabel :: ( Maybe Int )
              _statOdStar :: DStar
              _condIexpValSpace :: ( PtConstLat -> ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIbreakLabels :: ( Set Int )
              _statIcontinueLabel :: ( Maybe Int )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set Edge )
              _statIinit :: Int
              _statIinterflow :: ( Set Inter )
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( DifTrans (Set String) )
              _statIvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty =
                  ({-# LINE 413 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2491 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 416 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2496 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 417 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2501 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 418 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2506 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 419 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2511 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 420 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2516 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 421 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2521 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 422 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2526 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 423 "AttributeGrammar.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _statIstrongLive
                   {-# LINE 2531 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 424 "AttributeGrammar.ag" #-}
                   insertL labelc_ id forwardAnalysis <> _statIvalSpace
                   {-# LINE 2536 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2541 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2550 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2555 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramsIexpValSpace :: ( [PtConstLat -> ConstLat] )
              _paramsIfreeVars :: ( [Set String] )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOpretty =
                  ({-# LINE 427 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2590 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 428 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2595 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 429 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2600 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 430 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2605 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 431 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2610 "AttributeGrammar.hs" #-}
                   )
              _proc =
                  ({-# LINE 432 "AttributeGrammar.ag" #-}
                   findProc name_ _lhsIdStar
                   {-# LINE 2615 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 433 "AttributeGrammar.ag" #-}
                   singleton (Inter labelCall_ (procEntry _proc    ) (procExit _proc    ) labelReturn_)
                   {-# LINE 2620 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 434 "AttributeGrammar.ag" #-}
                   fromList [(labelCall_, (procEntry _proc    )), ((procExit _proc    ), labelReturn_)]
                   {-# LINE 2625 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 435 "AttributeGrammar.ag" #-}
                   insertL labelReturn_ (retStrong out_ $ procOut _proc    )    $ singleR labelCall_ (callStrong (procInp _proc    ) _paramsIfreeVars)
                   {-# LINE 2630 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 436 "AttributeGrammar.ag" #-}
                   insertL labelCall_ (callConst (procInp _proc    ) $ _paramsIexpValSpace) $ singleR labelReturn_ (retConst out_ $ procOut _proc    )
                   {-# LINE 2635 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2640 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2649 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIexpValSpace,_paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( PtConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 439 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2681 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 440 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2686 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 441 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2691 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 442 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2696 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 443 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2701 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 444 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2706 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 445 "AttributeGrammar.ag" #-}
                   singleL label_ (survive name_ _valIfreeVars)
                   {-# LINE 2711 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 446 "AttributeGrammar.ag" #-}
                   singleL label_ (updateConst name_ _valIexpValSpace)
                   {-# LINE 2716 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2721 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2726 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2735 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( PtConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOpretty =
                  ({-# LINE 449 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2767 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 450 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2772 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 451 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2777 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 452 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2782 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 453 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2787 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 454 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2792 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 455 "AttributeGrammar.ag" #-}
                   singleL label_ (survive name_ _valIfreeVars)
                   {-# LINE 2797 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 456 "AttributeGrammar.ag" #-}
                   singleL label_ (updateConst name_ _valIexpValSpace)
                   {-# LINE 2802 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2807 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2812 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2821 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _stat1IbreakLabels :: ( Set Int )
              _stat1IcontinueLabel :: ( Maybe Int )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set Edge )
              _stat1Iinit :: Int
              _stat1Iinterflow :: ( Set Inter )
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( DifTrans (Set String) )
              _stat1IvalSpace :: ( DifTrans PtConstLat )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IcontinueLabel :: ( Maybe Int )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set Edge )
              _stat2Iinit :: Int
              _stat2Iinterflow :: ( Set Inter )
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( DifTrans (Set String) )
              _stat2IvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty =
                  ({-# LINE 459 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2875 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 460 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2880 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 461 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2885 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 462 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2890 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 463 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2895 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 464 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2900 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2905 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2910 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2915 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2920 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2929 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2934 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2939 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2944 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2949 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _sizeIexpValSpace :: ( PtConstLat -> ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 467 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2983 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 468 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2988 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 469 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2993 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 470 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2998 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 471 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3003 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 472 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3008 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 473 "AttributeGrammar.ag" #-}
                   singleL label_ (survive name_ _sizeIfreeVars)
                   {-# LINE 3013 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 475 "AttributeGrammar.ag" #-}
                   insertL label_ id forwardAnalysis
                   {-# LINE 3018 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3023 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3028 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3037 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIexpValSpace,_sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( PtConstLat -> ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 479 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3068 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 480 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3073 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 481 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3078 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 482 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3083 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 483 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3088 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 484 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3093 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 485 "AttributeGrammar.ag" #-}
                   insertL label_ id backwardAnalysis
                   {-# LINE 3098 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 486 "AttributeGrammar.ag" #-}
                   insertL label_ id forwardAnalysis
                   {-# LINE 3103 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3108 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3113 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3122 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( PtConstLat -> ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIexpValSpace :: ( PtConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 489 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3159 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 490 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3164 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 491 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3169 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 492 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3174 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 493 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3179 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 494 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3184 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 496 "AttributeGrammar.ag" #-}
                   insertL label_ id backwardAnalysis
                   {-# LINE 3189 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 497 "AttributeGrammar.ag" #-}
                   insertL label_ id forwardAnalysis
                   {-# LINE 3194 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3199 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3204 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3213 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 500 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3240 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 501 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3245 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 502 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3250 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 503 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3255 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 504 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3260 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 505 "AttributeGrammar.ag" #-}
                   singleton (label_, (Maybe.fromJust _lhsIcontinueLabel))
                   {-# LINE 3265 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 506 "AttributeGrammar.ag" #-}
                   insertL label_ id backwardAnalysis
                   {-# LINE 3270 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 507 "AttributeGrammar.ag" #-}
                   insertL label_ id forwardAnalysis
                   {-# LINE 3275 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3280 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3285 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3294 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 510 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3317 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 511 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3322 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 512 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3327 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 513 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3332 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 514 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3337 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 515 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3342 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 516 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3347 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 517 "AttributeGrammar.ag" #-}
                   insertL label_ id backwardAnalysis
                   {-# LINE 3352 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 518 "AttributeGrammar.ag" #-}
                   insertL label_ id forwardAnalysis
                   {-# LINE 3357 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3362 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3371 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))