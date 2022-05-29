

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "StrongLive.ag" #-}

import StrongLive
{-# LINE 9 "AttributeGrammar.hs" #-}

{-# LINE 1 "ConstantProp.ag" #-}

import ConstantProp
{-# LINE 14 "AttributeGrammar.hs" #-}

{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.Set

import Std((.:))

import Analyses
{-# LINE 26 "AttributeGrammar.hs" #-}

{-# LINE 208 "AttributeGrammar.ag" #-}

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
{-# LINE 48 "AttributeGrammar.hs" #-}

{-# LINE 366 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 55 "AttributeGrammar.hs" #-}
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
             ({-# LINE 66 "ConstantProp.ag" #-}
              \_ -> Just $ CB val_
              {-# LINE 115 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 398 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 120 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 399 "AttributeGrammar.ag" #-}
              10
              {-# LINE 125 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 130 "AttributeGrammar.hs" #-}
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
             ({-# LINE 68 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 148 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 401 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 153 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 402 "AttributeGrammar.ag" #-}
              10
              {-# LINE 158 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 163 "AttributeGrammar.hs" #-}
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
             ({-# LINE 70 "ConstantProp.ag" #-}
              \env -> cIIB (<) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 192 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 197 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 406 "AttributeGrammar.ag" #-}
              4
              {-# LINE 202 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 207 "AttributeGrammar.hs" #-}
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
             ({-# LINE 72 "ConstantProp.ag" #-}
              \env -> cIIB (>) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 240 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 408 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 245 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
              4
              {-# LINE 250 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 255 "AttributeGrammar.hs" #-}
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
             ({-# LINE 74 "ConstantProp.ag" #-}
              \env -> cIIB (<=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 288 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 411 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 293 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              4
              {-# LINE 298 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 303 "AttributeGrammar.hs" #-}
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
             ({-# LINE 76 "ConstantProp.ag" #-}
              \env -> cIIB (>=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 336 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 341 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 415 "AttributeGrammar.ag" #-}
              4
              {-# LINE 346 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 351 "AttributeGrammar.hs" #-}
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
             ({-# LINE 78 "ConstantProp.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 384 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 389 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 418 "AttributeGrammar.ag" #-}
              4
              {-# LINE 394 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 399 "AttributeGrammar.hs" #-}
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
             ({-# LINE 80 "ConstantProp.ag" #-}
              \env -> cBBB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 432 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 437 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              4
              {-# LINE 442 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 447 "AttributeGrammar.hs" #-}
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
             ({-# LINE 82 "ConstantProp.ag" #-}
              \env -> constAnd (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 480 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 485 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 424 "AttributeGrammar.ag" #-}
              3
              {-# LINE 490 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 495 "AttributeGrammar.hs" #-}
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
             ({-# LINE 84 "ConstantProp.ag" #-}
              \env -> constOr (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 528 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 533 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 427 "AttributeGrammar.ag" #-}
              2
              {-# LINE 538 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 543 "AttributeGrammar.hs" #-}
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
             ({-# LINE 86 "ConstantProp.ag" #-}
              \env -> cBB not (_valIexpValSpace env)
              {-# LINE 570 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 575 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
              10
              {-# LINE 580 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 585 "AttributeGrammar.hs" #-}
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
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 630 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 203 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 635 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 644 "AttributeGrammar.hs" #-}
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
             ({-# LINE 436 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 664 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 203 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 669 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 678 "AttributeGrammar.hs" #-}
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
             ({-# LINE 442 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 719 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              _hdIexpValSpace : _tlIexpValSpace
              {-# LINE 724 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              _hdIfreeVars : _tlIfreeVars
              {-# LINE 729 "AttributeGrammar.hs" #-}
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
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 749 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              []
              {-# LINE 754 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              []
              {-# LINE 759 "AttributeGrammar.hs" #-}
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
             ({-# LINE 51 "ConstantProp.ag" #-}
              \_ -> Just $ CI val_
              {-# LINE 813 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 374 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 818 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 375 "AttributeGrammar.ag" #-}
              10
              {-# LINE 823 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 828 "AttributeGrammar.hs" #-}
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
             ({-# LINE 53 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 846 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 377 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 851 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 378 "AttributeGrammar.ag" #-}
              10
              {-# LINE 856 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 379 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 861 "AttributeGrammar.hs" #-}
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
             ({-# LINE 55 "ConstantProp.ag" #-}
              \env -> cIII (+) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 890 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 381 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 895 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 382 "AttributeGrammar.ag" #-}
              6
              {-# LINE 900 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 905 "AttributeGrammar.hs" #-}
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
             ({-# LINE 57 "ConstantProp.ag" #-}
              \env -> cIII (-) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 938 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 384 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 943 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 385 "AttributeGrammar.ag" #-}
              6
              {-# LINE 948 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 953 "AttributeGrammar.hs" #-}
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
             ({-# LINE 59 "ConstantProp.ag" #-}
              \env -> constMul (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 986 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 387 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 991 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 388 "AttributeGrammar.ag" #-}
              7
              {-# LINE 996 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1001 "AttributeGrammar.hs" #-}
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
             ({-# LINE 61 "ConstantProp.ag" #-}
              \env -> cIII div (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1034 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 390 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1039 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 391 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1044 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1049 "AttributeGrammar.hs" #-}
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
             ({-# LINE 393 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1076 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1081 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1086 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _ptrIexpValSpace
              {-# LINE 1095 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1137 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 140 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1142 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1147 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   (name_, Proc'' _lhsIlabel _statIlabel name_ inp_ out_)
                   {-# LINE 1152 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOstrongLive =
                  ({-# LINE 20 "StrongLive.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIstrongLive
                   {-# LINE 1214 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 21 "ConstantProp.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIvalSpace
                   {-# LINE 1219 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1226 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 260 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1231 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 261 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1236 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1241 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 263 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1246 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 264 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1251 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1256 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1305 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   _hdIdStar : _tlIdStar
                   {-# LINE 1310 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1319 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1324 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1329 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1346 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1351 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1360 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
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
              _lhsOstrongLive =
                  ({-# LINE 16 "StrongLive.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1411 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 17 "ConstantProp.ag" #-}
                   _hdIvalSpace <> _tlIvalSpace
                   {-# LINE 1416 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 251 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1421 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1426 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   _hdIinterflow <> _tlIinterflow
                   {-# LINE 1431 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1440 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 201 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1445 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIpretty,_hdIself,_hdIstrongLive,_hdIvalSpace) =
                  hd_ _hdOdStar
              ( _tlIflow,_tlIinterflow,_tlIpretty,_tlIself,_tlIstrongLive,_tlIvalSpace) =
                  tl_ _tlOdStar
          in  ( _lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Procs'
              _lhsOstrongLive =
                  ({-# LINE 14 "StrongLive.ag" #-}
                   mempty
                   {-# LINE 1464 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 15 "ConstantProp.ag" #-}
                   mempty
                   {-# LINE 1469 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1474 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 248 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1479 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 249 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1484 "AttributeGrammar.hs" #-}
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
             ({-# LINE 125 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1527 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 126 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1532 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 127 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1537 "AttributeGrammar.hs" #-}
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
    (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
         _lhsOstrongLive =
             ({-# LINE 10 "StrongLive.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1603 "AttributeGrammar.hs" #-}
              )
         _lhsOvalSpace =
             ({-# LINE 11 "ConstantProp.ag" #-}
              _procsIvalSpace <> _statIvalSpace
              {-# LINE 1608 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 232 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1613 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 235 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1618 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 236 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1623 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 237 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1628 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 238 "AttributeGrammar.ag" #-}
              _procsIinterflow <> _statIinterflow
              {-# LINE 1633 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1638 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1643 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 243 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1648 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
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
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1720 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1725 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1752 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 151 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1757 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1762 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1767 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1793 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1798 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1803 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1824 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1829 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1847 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 166 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1852 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 169 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1870 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1875 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 173 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1901 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1906 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 175 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1911 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1920 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1938 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1943 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1960 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1965 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1983 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 187 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1988 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2004 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2009 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2025 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2030 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 24 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 2110 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 25 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2115 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2120 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 270 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2125 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2130 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 272 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2135 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 273 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2140 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 274 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2145 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2150 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2155 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2164 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
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
              _lhsOstrongLive =
                  ({-# LINE 26 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2223 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 27 "ConstantProp.ag" #-}
                   insertL labelc_ id $ _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2228 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2239 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 283 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2244 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 284 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2249 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2254 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 286 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2259 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 287 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2264 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2269 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2274 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2283 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2288 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2293 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2298 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2303 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOvalSpace :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _statOcontinueLabel :: ( Maybe Int )
              _statOdStar :: DStar
              _condIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
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
              _lhsOstrongLive =
                  ({-# LINE 28 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _statIstrongLive
                   {-# LINE 2353 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 29 "ConstantProp.ag" #-}
                   insertL labelc_ id _statIvalSpace
                   {-# LINE 2358 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2365 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 292 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2370 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 293 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2375 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 294 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2380 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 295 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2385 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 296 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2390 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2395 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2400 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2405 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2414 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2419 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramsIexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
              _paramsIfreeVars :: ( [Set String] )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOstrongLive =
                  ({-# LINE 30 "StrongLive.ag" #-}
                   insertL labelReturn_ (aliveInExit out_ (procOut _proc    )) $ singleR labelCall_ (aliveInCall (procInp _proc    ) (procOut _proc    ) _paramsIfreeVars)
                   {-# LINE 2454 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 31 "ConstantProp.ag" #-}
                   insertL labelCall_ (fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIexpValSpace) $ singleR labelReturn_ ((<*>) .: fmap $ retConst out_ (procInp _proc    ) (procOut _proc    ))
                   {-# LINE 2459 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2464 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 302 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2474 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2479 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2484 "AttributeGrammar.hs" #-}
                   )
              _proc =
                  ({-# LINE 305 "AttributeGrammar.ag" #-}
                   findProc name_ _lhsIdStar
                   {-# LINE 2489 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   singleton (Inter labelCall_ (procEntry _proc    ) (procExit _proc    ) labelReturn_)
                   {-# LINE 2494 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   fromList [(labelCall_, (procEntry _proc    )), ((procExit _proc    ), labelReturn_)]
                   {-# LINE 2499 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2504 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2513 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 32 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2545 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 33 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2550 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2555 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 310 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2560 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2565 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 312 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2570 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2575 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2580 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2585 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2590 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2599 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOstrongLive =
                  ({-# LINE 34 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2631 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 35 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2636 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2641 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2646 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 318 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2651 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 319 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2656 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2661 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2666 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2671 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2676 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2685 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOstrongLive =
                  ({-# LINE 36 "StrongLive.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2739 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 37 "ConstantProp.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2744 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2749 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2754 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2759 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2764 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2769 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2774 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2779 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2784 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2793 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2798 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2803 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2808 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2813 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _sizeIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 38 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _sizeIfreeVars)
                   {-# LINE 2847 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 39 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2852 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2857 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2862 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2867 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2872 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2877 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2882 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2887 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2892 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2901 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 40 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 2932 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 41 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2937 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2942 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2947 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2952 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 340 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2957 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2962 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2967 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2972 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2977 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2986 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
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
                  ({-# LINE 42 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3023 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 43 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3028 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3033 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3038 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3043 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3048 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3053 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 349 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3058 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3063 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3068 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3077 "AttributeGrammar.hs" #-}
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
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 44 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3104 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 45 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3109 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 351 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3114 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3119 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3124 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3129 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3134 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   singleton (label_, (Maybe.fromJust _lhsIcontinueLabel))
                   {-# LINE 3139 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3144 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3149 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3158 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 46 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3181 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 47 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3186 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3191 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3196 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3201 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3206 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3211 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3216 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3221 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3226 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3235 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))