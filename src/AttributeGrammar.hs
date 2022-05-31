

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

import ConstantProp
{-# LINE 20 "AttributeGrammar.hs" #-}

{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.Set

import Std((.:))

import Analyses
{-# LINE 32 "AttributeGrammar.hs" #-}

{-# LINE 214 "AttributeGrammar.ag" #-}

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
{-# LINE 54 "AttributeGrammar.hs" #-}

{-# LINE 379 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 61 "AttributeGrammar.hs" #-}
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
              {-# LINE 121 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 411 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 126 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              10
              {-# LINE 131 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 136 "AttributeGrammar.hs" #-}
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
              {-# LINE 154 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 159 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 415 "AttributeGrammar.ag" #-}
              10
              {-# LINE 164 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 416 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 169 "AttributeGrammar.hs" #-}
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
              {-# LINE 198 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 418 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 203 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 419 "AttributeGrammar.ag" #-}
              4
              {-# LINE 208 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 213 "AttributeGrammar.hs" #-}
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
              {-# LINE 246 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 251 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 422 "AttributeGrammar.ag" #-}
              4
              {-# LINE 256 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 261 "AttributeGrammar.hs" #-}
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
              {-# LINE 294 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 424 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 299 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              4
              {-# LINE 304 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 309 "AttributeGrammar.hs" #-}
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
              {-# LINE 342 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 427 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 347 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              4
              {-# LINE 352 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 357 "AttributeGrammar.hs" #-}
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
              {-# LINE 390 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 395 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              4
              {-# LINE 400 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 405 "AttributeGrammar.hs" #-}
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
              {-# LINE 438 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 433 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 443 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              4
              {-# LINE 448 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 453 "AttributeGrammar.hs" #-}
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
              {-# LINE 486 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 436 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 491 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 437 "AttributeGrammar.ag" #-}
              3
              {-# LINE 496 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 501 "AttributeGrammar.hs" #-}
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
              {-# LINE 534 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 439 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 539 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              2
              {-# LINE 544 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 549 "AttributeGrammar.hs" #-}
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
              {-# LINE 576 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 442 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 581 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 443 "AttributeGrammar.ag" #-}
              10
              {-# LINE 586 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 591 "AttributeGrammar.hs" #-}
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
             ({-# LINE 447 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 636 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 209 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 641 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 650 "AttributeGrammar.hs" #-}
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
             ({-# LINE 449 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 670 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 209 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 675 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIexpValSpace
              {-# LINE 684 "AttributeGrammar.hs" #-}
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
             ({-# LINE 455 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 725 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              _hdIexpValSpace : _tlIexpValSpace
              {-# LINE 730 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 210 "AttributeGrammar.ag" #-}
              _hdIfreeVars : _tlIfreeVars
              {-# LINE 735 "AttributeGrammar.hs" #-}
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
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 755 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 7 "ConstantProp.ag" #-}
              []
              {-# LINE 760 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 210 "AttributeGrammar.ag" #-}
              []
              {-# LINE 765 "AttributeGrammar.hs" #-}
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
              {-# LINE 819 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 387 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 824 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 388 "AttributeGrammar.ag" #-}
              10
              {-# LINE 829 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 834 "AttributeGrammar.hs" #-}
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
              {-# LINE 852 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 390 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 857 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 391 "AttributeGrammar.ag" #-}
              10
              {-# LINE 862 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 392 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 867 "AttributeGrammar.hs" #-}
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
              {-# LINE 896 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 901 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 395 "AttributeGrammar.ag" #-}
              6
              {-# LINE 906 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 911 "AttributeGrammar.hs" #-}
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
              {-# LINE 944 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 397 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 949 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 398 "AttributeGrammar.ag" #-}
              6
              {-# LINE 954 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 959 "AttributeGrammar.hs" #-}
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
              {-# LINE 992 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 400 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 997 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 401 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1002 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1007 "AttributeGrammar.hs" #-}
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
              {-# LINE 1040 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1045 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 404 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1050 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1055 "AttributeGrammar.hs" #-}
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
             ({-# LINE 406 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1082 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 407 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1087 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1092 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _ptrIexpValSpace
              {-# LINE 1101 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1143 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1148 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 143 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1153 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 144 "AttributeGrammar.ag" #-}
                   (name_, Proc'' _lhsIlabel _statIlabel name_ inp_ out_)
                   {-# LINE 1158 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1222 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 22 "ConstantBranch.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIconstBranchT
                   {-# LINE 1227 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 21 "ConstantProp.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIvalSpace
                   {-# LINE 1232 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 263 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1239 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1244 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1249 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1254 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1259 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 270 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1264 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1269 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIbreakLabels,_statIconstBranchT,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
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
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 1318 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   _hdIdStar : _tlIdStar
                   {-# LINE 1323 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1332 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1337 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1342 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 133 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1359 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1364 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1373 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1427 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 18 "ConstantBranch.ag" #-}
                   _hdIconstBranchT <> _tlIconstBranchT
                   {-# LINE 1432 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 17 "ConstantProp.ag" #-}
                   _hdIvalSpace <> _tlIvalSpace
                   {-# LINE 1437 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1442 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 258 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1447 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   _hdIinterflow <> _tlIinterflow
                   {-# LINE 1452 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1461 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 207 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1466 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1486 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 16 "ConstantBranch.ag" #-}
                   mempty
                   {-# LINE 1491 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 15 "ConstantProp.ag" #-}
                   mempty
                   {-# LINE 1496 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1501 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 254 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1506 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1511 "AttributeGrammar.hs" #-}
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
             ({-# LINE 127 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1554 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 128 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1559 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 129 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1564 "AttributeGrammar.hs" #-}
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
              {-# LINE 1633 "AttributeGrammar.hs" #-}
              )
         _lhsOconstBranchT =
             ({-# LINE 12 "ConstantBranch.ag" #-}
              _procsIconstBranchT <> _statIconstBranchT
              {-# LINE 1638 "AttributeGrammar.hs" #-}
              )
         _lhsOvalSpace =
             ({-# LINE 11 "ConstantProp.ag" #-}
              _procsIvalSpace <> _statIvalSpace
              {-# LINE 1643 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 238 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1648 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1653 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1658 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 243 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1663 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 244 "AttributeGrammar.ag" #-}
              _procsIinterflow <> _statIinterflow
              {-# LINE 1668 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 247 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1673 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 248 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1678 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 249 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1683 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         ( _procsIconstBranchT,_procsIflow,_procsIinterflow,_procsIpretty,_procsIself,_procsIstrongLive,_procsIvalSpace) =
             procs_ _procsOdStar
         ( _statIbreakLabels,_statIconstBranchT,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
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
                  ({-# LINE 148 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1758 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 149 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1763 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1790 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1795 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 154 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1800 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 155 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1805 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1831 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 159 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1836 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 160 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1841 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 163 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1860 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 164 "AttributeGrammar.ag" #-}
                   Print' _lhsIlabel param_
                   {-# LINE 1865 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 167 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1884 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1889 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 171 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1907 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 172 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1912 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 175 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1930 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 176 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1935 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1961 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 180 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1966 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 181 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1971 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1980 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 184 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1998 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 185 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 2003 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 188 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2020 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2025 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 192 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2043 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 193 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2048 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 196 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2064 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 197 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2069 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 200 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 2085 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 201 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2090 "AttributeGrammar.hs" #-}
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
               ( ( Set Int ),( DifTrans ConstBranchLat ),( Maybe Int ),( Set Int ),( Set Edge ),Int,( Set Inter ),Bool,Bool,( [String] ),Stat',( DifTrans (Set String) ),( DifTrans PtConstLat ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: DStar}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),constBranchT_Syn_Stat' :: ( DifTrans ConstBranchLat ),continueLabel_Syn_Stat' :: ( Maybe Int ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set Edge ),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ( Set Inter ),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( DifTrans (Set String) ),valSpace_Syn_Stat' :: ( DifTrans PtConstLat )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar) =
    (let ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIcontinueLabel _lhsIdStar
     in  (Syn_Stat' _lhsObreakLabels _lhsOconstBranchT _lhsOcontinueLabel _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 24 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 2174 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 26 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2179 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 25 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2184 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2189 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2194 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2199 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2204 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2209 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 280 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2214 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2219 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2224 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2233 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
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
                   {-# LINE 2295 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 28 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchIf (_condIexpValSpace) _stat1Iinit _stat2Iinit) $ _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2300 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 27 "ConstantProp.ag" #-}
                   insertL labelc_ id $ _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2305 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 282 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2316 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2321 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2326 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2331 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 292 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2336 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 293 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2341 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2346 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2351 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2360 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2365 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2370 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2375 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2380 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _statIconstBranchT :: ( DifTrans ConstBranchLat )
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
                   {-# LINE 2432 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 30 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchWhile (_condIexpValSpace) _statIinit labelc_)  _statIconstBranchT
                   {-# LINE 2437 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 29 "ConstantProp.ag" #-}
                   insertL labelc_ id _statIvalSpace
                   {-# LINE 2442 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 295 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2449 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2454 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2459 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2464 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 302 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2474 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2479 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2484 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2489 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2498 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2503 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIconstBranchT,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _paramIfreeVars :: ( Set String )
              _paramIpretty :: String
              _paramIself :: Expr
              _lhsOstrongLive =
                  ({-# LINE 30 "StrongLive.ag" #-}
                   singleL label_ (<> _paramIfreeVars)
                   {-# LINE 2536 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 32 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2541 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 31 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2546 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   ["print(" ++ _paramIpretty ++ ")"]
                   {-# LINE 2551 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2556 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 308 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2561 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2566 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 310 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2571 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2576 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2581 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2586 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Print' label_ _paramIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2595 "AttributeGrammar.hs" #-}
                   )
              ( _paramIexpValSpace,_paramIfreeVars,_paramIpretty,_paramIself) =
                  param_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramsIexpValSpace :: ( [ConstEnv -> Maybe ConstLat] )
              _paramsIfreeVars :: ( [Set String] )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOstrongLive =
                  ({-# LINE 32 "StrongLive.ag" #-}
                   insertL labelReturn_ (aliveInExit out_ (procOut _proc    )) $ singleR labelCall_ (aliveInCall (procInp _proc    ) (procOut _proc    ) _paramsIfreeVars)
                   {-# LINE 2629 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 34 "ConstantBranch.ag" #-}
                   insertL labelCall_ (ignoreDead labelCall_ $ fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIexpValSpace) $ singleR labelReturn_ (ignoreDead2 labelReturn_ $ ((<*>) .: fmap $ retConst out_ (procInp _proc    ) (procOut _proc    )))
                   {-# LINE 2634 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 33 "ConstantProp.ag" #-}
                   insertL labelCall_ (fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIexpValSpace) $ singleR labelReturn_ ((<*>) .: fmap $ retConst out_ (procInp _proc    ) (procOut _proc    ))
                   {-# LINE 2639 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2644 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2649 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 315 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2654 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2659 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2664 "AttributeGrammar.hs" #-}
                   )
              _proc =
                  ({-# LINE 318 "AttributeGrammar.ag" #-}
                   findProc name_ _lhsIdStar
                   {-# LINE 2669 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 319 "AttributeGrammar.ag" #-}
                   singleton (Inter labelCall_ (procEntry _proc    ) (procExit _proc    ) labelReturn_)
                   {-# LINE 2674 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   fromList [(labelCall_, (procEntry _proc    )), ((procExit _proc    ), labelReturn_)]
                   {-# LINE 2679 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2684 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2693 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIexpValSpace,_paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 34 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2726 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 36 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2731 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 35 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2736 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2741 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2746 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2751 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2756 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2761 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2766 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2771 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2776 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2785 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOstrongLive =
                  ({-# LINE 36 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2818 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 38 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2823 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 37 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIexpValSpace)
                   {-# LINE 2828 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2833 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2838 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2843 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2848 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2853 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2858 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2863 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2868 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2877 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _stat1IbreakLabels :: ( Set Int )
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
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
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
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
                  ({-# LINE 38 "StrongLive.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2934 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 40 "ConstantBranch.ag" #-}
                   _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2939 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 39 "ConstantProp.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2944 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2949 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2954 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2959 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2964 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 340 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2969 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2974 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2979 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2984 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2993 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2998 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 3003 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 3008 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 3013 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _sizeIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 40 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _sizeIfreeVars)
                   {-# LINE 3048 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 42 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3053 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 41 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3058 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3063 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3068 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3073 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3078 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3083 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3088 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3093 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3098 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3107 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIexpValSpace,_sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 42 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3139 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 44 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3144 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 43 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3149 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 350 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3154 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 351 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3159 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3164 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3169 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3174 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3179 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3184 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3189 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3198 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
                  ({-# LINE 44 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3236 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 46 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3241 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 45 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3246 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 357 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3251 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 358 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3256 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3261 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3266 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3271 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3276 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3281 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3286 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3295 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 46 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3323 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 48 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3328 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 47 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3333 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3338 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3343 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 366 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3348 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 367 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3353 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3358 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   singleton (label_, (Maybe.fromJust _lhsIcontinueLabel))
                   {-# LINE 3363 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3368 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3373 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3382 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive =
                  ({-# LINE 48 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3406 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 50 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3411 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 49 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3416 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3421 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3426 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 373 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3431 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3436 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3441 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 376 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3446 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 377 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3451 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 206 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3456 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 212 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3465 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))