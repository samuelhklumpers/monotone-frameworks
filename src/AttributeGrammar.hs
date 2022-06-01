

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "StrongLive.ag" #-}

import StrongLive
{-# LINE 9 "AttributeGrammar.hs" #-}

{-# LINE 3 "ConstantBranch.ag" #-}

import ConstantProp
import ConstantBranch
{-# LINE 15 "AttributeGrammar.hs" #-}

{-# LINE 1 "ConstantProp.ag" #-}

import Std (liftA2)
{-# LINE 20 "AttributeGrammar.hs" #-}

{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Maybe as Maybe
import Data.Set

import Std((.:))

import Analyses
{-# LINE 30 "AttributeGrammar.hs" #-}

{-# LINE 212 "AttributeGrammar.ag" #-}

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
{-# LINE 52 "AttributeGrammar.hs" #-}

{-# LINE 377 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 59 "AttributeGrammar.hs" #-}
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
type T_BExpr = ( ( ConstEnv -> Maybe ConstLat ),( Set String ),Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {expValSpace_Syn_BExpr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_BExpr :: ( Set String ),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOexpValSpace _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 68 "ConstantProp.ag" #-}
              \_ -> Just $ CB val_
              {-# LINE 119 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 124 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 410 "AttributeGrammar.ag" #-}
              10
              {-# LINE 129 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 134 "AttributeGrammar.hs" #-}
              )
         _self =
             BConst val_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 70 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 152 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 157 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 413 "AttributeGrammar.ag" #-}
              10
              {-# LINE 162 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 167 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 72 "ConstantProp.ag" #-}
              \env -> cIIB (<) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 196 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 416 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 201 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              4
              {-# LINE 206 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 211 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 74 "ConstantProp.ag" #-}
              \env -> cIIB (>) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 244 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 419 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 249 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              4
              {-# LINE 254 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 259 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 76 "ConstantProp.ag" #-}
              \env -> cIIB (<=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 292 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 422 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 297 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              4
              {-# LINE 302 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 307 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 78 "ConstantProp.ag" #-}
              \env -> cIIB (>=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 340 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 345 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              4
              {-# LINE 350 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 355 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 80 "ConstantProp.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 388 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 393 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
              4
              {-# LINE 398 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 403 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 82 "ConstantProp.ag" #-}
              \env -> cBBB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 436 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 441 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 432 "AttributeGrammar.ag" #-}
              4
              {-# LINE 446 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 451 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 84 "ConstantProp.ag" #-}
              \env -> constAnd (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 484 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 489 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 435 "AttributeGrammar.ag" #-}
              3
              {-# LINE 494 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 499 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 86 "ConstantProp.ag" #-}
              \env -> constOr (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 532 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 437 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 537 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 438 "AttributeGrammar.ag" #-}
              2
              {-# LINE 542 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 547 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _valIfreeVars :: ( Set String )
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOexpValSpace =
             ({-# LINE 88 "ConstantProp.ag" #-}
              \env -> cBB not (_valIexpValSpace env)
              {-# LINE 574 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 579 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 441 "AttributeGrammar.ag" #-}
              10
              {-# LINE 584 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 589 "AttributeGrammar.hs" #-}
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
type T_Expr = ( ( ConstEnv -> Maybe ConstLat ),( Set String ),String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {expValSpace_Syn_Expr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_Expr :: ( Set String ),pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
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
         _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _exprIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 445 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 634 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 207 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 639 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 648 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _exprIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 447 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 668 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 207 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 673 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 682 "AttributeGrammar.hs" #-}
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
type T_Exprs = ( ( [ConstEnv -> Maybe ConstLat] ),( [Set String] ),String,Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {expValSpace_Syn_Exprs :: ( [ConstEnv -> Maybe ConstLat] ),freeVars_Syn_Exprs :: ( [Set String] ),pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
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
         _lhsOexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _hdIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _hdIfreeVars :: ( Set String )
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
         _tlIfreeVars :: ( [Set String] )
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 723 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              _hdIexpValSpace : _tlIexpValSpace
              {-# LINE 728 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _hdIfreeVars : _tlIfreeVars
              {-# LINE 733 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 451 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 753 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              []
              {-# LINE 758 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              []
              {-# LINE 763 "AttributeGrammar.hs" #-}
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
type T_IExpr = ( ( ConstEnv -> Maybe ConstLat ),( Set String ),Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {expValSpace_Syn_IExpr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_IExpr :: ( Set String ),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOexpValSpace _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 53 "ConstantProp.ag" #-}
              \_ -> Just $ CI val_
              {-# LINE 817 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 385 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 822 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 386 "AttributeGrammar.ag" #-}
              10
              {-# LINE 827 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 832 "AttributeGrammar.hs" #-}
              )
         _self =
             IConst val_
         _lhsOself =
             _self
     in  ( _lhsOexpValSpace,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 55 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 850 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 388 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 855 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 389 "AttributeGrammar.ag" #-}
              10
              {-# LINE 860 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 390 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 865 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 57 "ConstantProp.ag" #-}
              \env -> cIII (+) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 894 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 392 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 899 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 393 "AttributeGrammar.ag" #-}
              6
              {-# LINE 904 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 909 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 59 "ConstantProp.ag" #-}
              \env -> cIII (-) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 942 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 395 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 947 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 396 "AttributeGrammar.ag" #-}
              6
              {-# LINE 952 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 957 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 61 "ConstantProp.ag" #-}
              \env -> constMul (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 990 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 398 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 995 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 399 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1000 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1005 "AttributeGrammar.hs" #-}
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
    (let _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOexpValSpace =
             ({-# LINE 63 "ConstantProp.ag" #-}
              \env -> cIII div (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1038 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 401 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1043 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 402 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1048 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1053 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _ptrIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
         _ptrIfreeVars :: ( Set String )
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 404 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1080 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1085 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 206 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1090 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _ptrIexpValSpace
              {-# LINE 1099 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 139 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1141 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 140 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1146 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1151 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   (name_, Proc'' _lhsIlabel _statIlabel name_ inp_ out_)
                   {-# LINE 1156 "AttributeGrammar.hs" #-}
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
               ( ( DifTrans ConstBranchLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),( [String] ),Proc',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: DStar}
data Syn_Proc' = Syn_Proc' {constBranchT_Syn_Proc' :: ( DifTrans ConstBranchLat ),final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set Edge ),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ( Set Inter ),pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc',strongLive_Syn_Proc' :: ( DifTrans (Set String) ),valSpace_Syn_Proc' :: ( DifTrans PtConstLat )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar) =
    (let ( _lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar
     in  (Syn_Proc' _lhsOconstBranchT _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelExit_ name_ inp_ out_ stat_ =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _statOdStar :: DStar
              _statOcontinueLabel :: ( Maybe Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Proc'
              _statIbreakLabels :: ( Set Int )
              _statIconstBranchT :: ( DifTrans ConstBranchLat )
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
              _lhsOstrongLive =
                  ({-# LINE 20 "StrongLive.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIstrongLive
                   {-# LINE 1219 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 22 "ConstantBranch.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIconstBranchT
                   {-# LINE 1224 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 21 "ConstantProp.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIvalSpace
                   {-# LINE 1229 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 261 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1236 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 264 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1241 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1246 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1251 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1256 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1261 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1266 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIbreakLabels,_statIconstBranchT,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1315 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   _hdIdStar : _tlIdStar
                   {-# LINE 1320 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1329 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1334 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1339 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 131 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1356 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1361 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1370 "AttributeGrammar.hs" #-}
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
                ( ( DifTrans ConstBranchLat ),( Set Edge ),( Set Inter ),( [String] ),Procs',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: DStar}
data Syn_Procs' = Syn_Procs' {constBranchT_Syn_Procs' :: ( DifTrans ConstBranchLat ),flow_Syn_Procs' :: ( Set Edge ),interflow_Syn_Procs' :: ( Set Inter ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs',strongLive_Syn_Procs' :: ( DifTrans (Set String) ),valSpace_Syn_Procs' :: ( DifTrans PtConstLat )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar) =
    (let ( _lhsOconstBranchT,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar
     in  (Syn_Procs' _lhsOconstBranchT _lhsOflow _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Procs'
              _hdOdStar :: DStar
              _tlOdStar :: DStar
              _hdIconstBranchT :: ( DifTrans ConstBranchLat )
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set Edge )
              _hdIinit :: Int
              _hdIinterflow :: ( Set Inter )
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _hdIstrongLive :: ( DifTrans (Set String) )
              _hdIvalSpace :: ( DifTrans PtConstLat )
              _tlIconstBranchT :: ( DifTrans ConstBranchLat )
              _tlIflow :: ( Set Edge )
              _tlIinterflow :: ( Set Inter )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _tlIstrongLive :: ( DifTrans (Set String) )
              _tlIvalSpace :: ( DifTrans PtConstLat )
              _lhsOstrongLive =
                  ({-# LINE 16 "StrongLive.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1424 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 18 "ConstantBranch.ag" #-}
                   _hdIconstBranchT <> _tlIconstBranchT
                   {-# LINE 1429 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 17 "ConstantProp.ag" #-}
                   _hdIvalSpace <> _tlIvalSpace
                   {-# LINE 1434 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1439 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 256 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1444 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   _hdIinterflow <> _tlIinterflow
                   {-# LINE 1449 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1458 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1463 "AttributeGrammar.hs" #-}
                   )
              ( _hdIconstBranchT,_hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIpretty,_hdIself,_hdIstrongLive,_hdIvalSpace) =
                  hd_ _hdOdStar
              ( _tlIconstBranchT,_tlIflow,_tlIinterflow,_tlIpretty,_tlIself,_tlIstrongLive,_tlIvalSpace) =
                  tl_ _tlOdStar
          in  ( _lhsOconstBranchT,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Procs'
              _lhsOstrongLive =
                  ({-# LINE 14 "StrongLive.ag" #-}
                   mempty
                   {-# LINE 1483 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 16 "ConstantBranch.ag" #-}
                   mempty
                   {-# LINE 1488 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 15 "ConstantProp.ag" #-}
                   mempty
                   {-# LINE 1493 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 251 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1498 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1503 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1508 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOconstBranchT,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
             ({-# LINE 125 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1551 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 126 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1556 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 127 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1561 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ( DifTrans ConstBranchLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),String,Program',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {constBranchT_Syn_Program' :: ( DifTrans ConstBranchLat ),final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set Edge ),init_Syn_Program' :: Int,interflow_Syn_Program' :: ( Set Inter ),pretty_Syn_Program' :: String,self_Syn_Program' :: Program',strongLive_Syn_Program' :: ( DifTrans (Set String) ),valSpace_Syn_Program' :: ( DifTrans PtConstLat )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem
     in  (Syn_Program' _lhsOconstBranchT _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         DStar ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOstrongLive :: ( DifTrans (Set String) )
         _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
         _lhsOvalSpace :: ( DifTrans PtConstLat )
         _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _lhsOflow :: ( Set Edge )
         _lhsOinterflow :: ( Set Inter )
         _statOdStar :: DStar
         _procsOdStar :: DStar
         _statOcontinueLabel :: ( Maybe Int )
         _lhsOself :: Program'
         _procsIconstBranchT :: ( DifTrans ConstBranchLat )
         _procsIflow :: ( Set Edge )
         _procsIinterflow :: ( Set Inter )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _procsIstrongLive :: ( DifTrans (Set String) )
         _procsIvalSpace :: ( DifTrans PtConstLat )
         _statIbreakLabels :: ( Set Int )
         _statIconstBranchT :: ( DifTrans ConstBranchLat )
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
         _lhsOstrongLive =
             ({-# LINE 10 "StrongLive.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1629 "AttributeGrammar.hs" #-}
              )
         _lhsOconstBranchT =
             ({-# LINE 12 "ConstantBranch.ag" #-}
              _procsIconstBranchT <> _statIconstBranchT
              {-# LINE 1634 "AttributeGrammar.hs" #-}
              )
         _lhsOvalSpace =
             ({-# LINE 11 "ConstantProp.ag" #-}
              _procsIvalSpace <> _statIvalSpace
              {-# LINE 1639 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 236 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1644 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 239 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1649 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 240 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1654 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1659 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              _procsIinterflow <> _statIinterflow
              {-# LINE 1664 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 245 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1669 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 246 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1674 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 247 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1679 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         ( _procsIconstBranchT,_procsIflow,_procsIinterflow,_procsIpretty,_procsIself,_procsIstrongLive,_procsIvalSpace) =
             procs_ _procsOdStar
         ( _statIbreakLabels,_statIconstBranchT,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
             stat_ _statOcontinueLabel _statOdStar
     in  ( _lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace))
-- Stat --------------------------------------------------------
data Stat = Skip
          | IfThenElse (BExpr) (Stat) (Stat)
          | While (BExpr) (Stat)
          | Print (Expr)
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
sem_Stat (Print _param) =
    (sem_Stat_Print _param)
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
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1754 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1759 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1786 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 151 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1791 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1796 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1801 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 156 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1827 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1832 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1837 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While cond_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Stat_Print :: Expr ->
                  T_Stat
sem_Stat_Print param_ =
    (\ _lhsIlabel ->
         (let _lhsOlabel :: Int
              _lhsOlabelled :: Stat'
              _lhsOself :: Stat
              _lhsOlabel =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1856 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   Print' _lhsIlabel param_
                   {-# LINE 1861 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Print param_
              _lhsOself =
                  _self
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
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1880 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 166 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1885 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 169 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1903 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1908 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 173 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1926 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1931 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1957 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1962 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1967 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1976 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1994 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1999 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2016 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 187 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2021 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2039 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2044 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2060 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2065 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2081 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2086 "AttributeGrammar.hs" #-}
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
           | Print' (Int) (Expr)
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
sem_Stat' (Print' _label _param) =
    (sem_Stat'_Print' _label (sem_Expr _param))
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
               ( ( Set Int ),( DifTrans ConstBranchLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),Bool,Bool,( [String] ),Stat',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: DStar}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),constBranchT_Syn_Stat' :: ( DifTrans ConstBranchLat ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set Edge ),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ( Set Inter ),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( DifTrans (Set String) ),valSpace_Syn_Stat' :: ( DifTrans PtConstLat )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar) =
    (let ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIcontinueLabel _lhsIdStar
     in  (Syn_Stat' _lhsObreakLabels _lhsOconstBranchT _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOstrongLive =
                  ({-# LINE 24 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 2169 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 26 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2174 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 25 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2179 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 273 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2184 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 274 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2189 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2194 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2199 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2204 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2209 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2214 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2219 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _condIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1IbreakLabels :: ( Set Int )
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _lhsOstrongLive =
                  ({-# LINE 26 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2282 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 28 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchIf (_condIexpValSpace) _stat1Iinit _stat2Iinit) $ _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2287 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 27 "ConstantProp.ag" #-}
                   insertL labelc_ id $ _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2292 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 280 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2303 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 287 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2308 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2313 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2318 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2323 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2328 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2333 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2338 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _stat1OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2347 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2352 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2357 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2362 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _statOcontinueLabel :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _statOdStar :: DStar
              _condIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIbreakLabels :: ( Set Int )
              _statIconstBranchT :: ( DifTrans ConstBranchLat )
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
              _lhsOstrongLive =
                  ({-# LINE 28 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _statIstrongLive
                   {-# LINE 2412 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 30 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchWhile (_condIexpValSpace) _statIinit labelc_)  _statIconstBranchT
                   {-# LINE 2417 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 29 "ConstantProp.ag" #-}
                   insertL labelc_ id _statIvalSpace
                   {-# LINE 2422 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 293 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2429 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 296 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2434 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2439 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2444 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2449 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2454 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2459 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 302 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2464 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2478 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIconstBranchT,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Print' :: Int ->
                    T_Expr ->
                    (T_Stat')
sem_Stat'_Print' label_ param_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _paramIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _paramIfreeVars :: ( Set String )
              _paramIpretty :: String
              _paramIself :: Expr
              _lhsOstrongLive =
                  ({-# LINE 30 "StrongLive.ag" #-}
                   singleL label_ (<> _paramIfreeVars)
                   {-# LINE 2510 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 32 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2515 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 31 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2520 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   ["print(" ++ _paramIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2525 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 305 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2530 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2535 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2540 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 308 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2545 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2550 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2555 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2560 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Print' label_ _paramIself
              _lhsOself =
                  _self
              ( _paramIexpValSpace,_paramIfreeVars,_paramIpretty,_paramIself) =
                  param_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _paramsIexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
              _paramsIfreeVars :: ( [Set String] )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOstrongLive =
                  ({-# LINE 32 "StrongLive.ag" #-}
                   insertL labelReturn_ (aliveInExit out_ (procOut _proc    )) $ singleR labelCall_ (aliveInCall (procInp _proc    ) (procOut _proc    ) _paramsIfreeVars)
                   {-# LINE 2597 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 34 "ConstantBranch.ag" #-}
                   insertL labelCall_ (ignoreDead labelCall_ $ fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIexpValSpace) $ singleR labelReturn_ (ignoreDead2 labelReturn_ $ ((<*>) .: fmap $ retConst out_ (procInp _proc    ) (procOut _proc    )))
                   {-# LINE 2602 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 33 "ConstantProp.ag" #-}
                   insertL labelCall_ (fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIexpValSpace) $ singleR labelReturn_ (liftA2 $ retConst out_ (procInp _proc    ) (procOut _proc    ))
                   {-# LINE 2607 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2612 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 312 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2617 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2622 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2627 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 315 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2632 "AttributeGrammar.hs" #-}
                   )
              _proc =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   findProc name_ _lhsIdStar
                   {-# LINE 2637 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   singleton (Inter labelCall_ (procEntry _proc    ) (procExit _proc    ) labelReturn_)
                   {-# LINE 2642 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 318 "AttributeGrammar.ag" #-}
                   fromList [(labelCall_, (procEntry _proc    )), ((procExit _proc    ), labelReturn_)]
                   {-# LINE 2647 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2652 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              ( _paramsIexpValSpace,_paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 34 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2688 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 36 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2693 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 35 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2698 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2703 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2708 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2713 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2718 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2723 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2728 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2733 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2738 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOstrongLive =
                  ({-# LINE 36 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2774 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 38 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2779 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 37 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2784 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2789 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2794 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2799 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2804 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2809 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2814 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2819 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2824 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _stat1IbreakLabels :: ( Set Int )
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _lhsOstrongLive =
                  ({-# LINE 38 "StrongLive.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2882 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 40 "ConstantBranch.ag" #-}
                   _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2887 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 39 "ConstantProp.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2892 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2897 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2902 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2907 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2912 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2917 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2922 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2927 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2932 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _stat1OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2941 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2946 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2951 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2956 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _sizeIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 40 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _sizeIfreeVars)
                   {-# LINE 2990 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 42 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2995 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 41 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3000 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3005 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3010 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3015 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3020 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3025 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3030 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3035 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3040 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              ( _sizeIexpValSpace,_sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _ptrIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 42 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3075 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 44 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3080 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 43 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3085 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3090 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 349 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3095 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 350 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3100 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 351 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3105 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3110 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3115 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3120 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3125 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _ptrIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 44 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3166 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 46 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3171 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 45 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3176 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3181 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3186 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 357 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3191 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3196 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3201 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3206 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3211 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3216 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOstrongLive =
                  ({-# LINE 46 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3247 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 48 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3252 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 47 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3257 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3262 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3267 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3272 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3277 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 366 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3282 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 367 "AttributeGrammar.ag" #-}
                   singleton (label_, (Maybe.fromMaybe (error $ "toplevel continue at " ++ show label_) _lhsIcontinueLabel))
                   {-# LINE 3287 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3292 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3297 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _lhsOstrongLive =
                  ({-# LINE 48 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3324 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 50 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3329 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 49 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3334 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3339 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 370 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3344 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3349 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3354 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 373 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3359 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3364 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3369 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3374 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))