

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

{-# LINE 381 "AttributeGrammar.ag" #-}

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
data Syn_BExpr = Syn_BExpr {constantPropagation_Syn_BExpr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_BExpr :: ( Set String ),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOconstantPropagation _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 68 "ConstantProp.ag" #-}
              \_ -> Just $ CB val_
              {-# LINE 119 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 413 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 124 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
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
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 70 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 152 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 416 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 157 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              10
              {-# LINE 162 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 418 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 167 "AttributeGrammar.hs" #-}
              )
         _self =
             BVar name_
         _lhsOself =
             _self
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 72 "ConstantProp.ag" #-}
              \env -> cIIB (<) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 196 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 201 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 74 "ConstantProp.ag" #-}
              \env -> cIIB (>) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 244 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 249 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 424 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 76 "ConstantProp.ag" #-}
              \env -> cIIB (<=) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 292 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 297 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 427 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 78 "ConstantProp.ag" #-}
              \env -> cIIB (>=) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 340 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 345 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 80 "ConstantProp.ag" #-}
              \env -> cIIB (==) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 388 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 432 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 393 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 433 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 82 "ConstantProp.ag" #-}
              \env -> cBBB (==) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 436 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 435 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 441 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 436 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 84 "ConstantProp.ag" #-}
              \env -> constAnd (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 484 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 438 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 489 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 439 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 86 "ConstantProp.ag" #-}
              \env -> constOr (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 532 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 441 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 537 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 442 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _valIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _valIfreeVars :: ( Set String )
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOconstantPropagation =
             ({-# LINE 88 "ConstantProp.ag" #-}
              \env -> cBB not (_valIconstantPropagation env)
              {-# LINE 574 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 444 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 579 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 445 "AttributeGrammar.ag" #-}
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
         ( _valIconstantPropagation,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
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
data Syn_Expr = Syn_Expr {constantPropagation_Syn_Expr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_Expr :: ( Set String ),pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Expr _lhsOconstantPropagation _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _exprIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 449 "AttributeGrammar.ag" #-}
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
         _lhsOconstantPropagation =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIconstantPropagation
              {-# LINE 648 "AttributeGrammar.hs" #-}
              )
         ( _exprIconstantPropagation,_exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _exprIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 451 "AttributeGrammar.ag" #-}
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
         _lhsOconstantPropagation =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _exprIconstantPropagation
              {-# LINE 682 "AttributeGrammar.hs" #-}
              )
         ( _exprIconstantPropagation,_exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself))
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
data Syn_Exprs = Syn_Exprs {constantPropagation_Syn_Exprs :: ( [ConstEnv -> Maybe ConstLat] ),freeVars_Syn_Exprs :: ( [Set String] ),pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Exprs _lhsOconstantPropagation _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _lhsOconstantPropagation :: ( [ConstEnv -> Maybe ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _hdIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _hdIfreeVars :: ( Set String )
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIconstantPropagation :: ( [ConstEnv -> Maybe ConstLat] )
         _tlIfreeVars :: ( [Set String] )
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 457 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 723 "AttributeGrammar.hs" #-}
              )
         _lhsOconstantPropagation =
             ({-# LINE 7 "ConstantProp.ag" #-}
              _hdIconstantPropagation : _tlIconstantPropagation
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
         ( _hdIconstantPropagation,_hdIfreeVars,_hdIpretty,_hdIself) =
             hd_
         ( _tlIconstantPropagation,_tlIfreeVars,_tlIpretty,_tlIself) =
             tl_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOconstantPropagation :: ( [ConstEnv -> Maybe ConstLat] )
         _lhsOfreeVars :: ( [Set String] )
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 455 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 753 "AttributeGrammar.hs" #-}
              )
         _lhsOconstantPropagation =
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
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOpretty,_lhsOself))
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
data Syn_IExpr = Syn_IExpr {constantPropagation_Syn_IExpr :: ( ConstEnv -> Maybe ConstLat ),freeVars_Syn_IExpr :: ( Set String ),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOconstantPropagation _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 53 "ConstantProp.ag" #-}
              \_ -> Just $ CI val_
              {-# LINE 817 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 389 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 822 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 390 "AttributeGrammar.ag" #-}
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
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 55 "ConstantProp.ag" #-}
              getConst name_
              {-# LINE 850 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 392 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 855 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 393 "AttributeGrammar.ag" #-}
              10
              {-# LINE 860 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 865 "AttributeGrammar.hs" #-}
              )
         _self =
             Var name_
         _lhsOself =
             _self
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 57 "ConstantProp.ag" #-}
              \env -> cIII (+) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 894 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 396 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 899 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 397 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 59 "ConstantProp.ag" #-}
              \env -> cIII (-) (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 942 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 399 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 947 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 400 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 61 "ConstantProp.ag" #-}
              \env -> constMul (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 990 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 402 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 995 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOconstantPropagation =
             ({-# LINE 63 "ConstantProp.ag" #-}
              \env -> cIII div (_leftIconstantPropagation env) (_rightIconstantPropagation env)
              {-# LINE 1038 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1043 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 406 "AttributeGrammar.ag" #-}
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
         ( _leftIconstantPropagation,_leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIconstantPropagation,_rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _ptrIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
         _ptrIfreeVars :: ( Set String )
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 408 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1080 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
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
         _lhsOconstantPropagation =
             ({-# LINE 6 "ConstantProp.ag" #-}
              _ptrIconstantPropagation
              {-# LINE 1099 "AttributeGrammar.hs" #-}
              )
         ( _ptrIconstantPropagation,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOconstantPropagation,_lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
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
               ( ( DifTrans ConstBranchLat ),( DifTrans PtConstLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),( [String] ),Proc',( DifTrans (Set String) ))
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: DStar}
data Syn_Proc' = Syn_Proc' {constBranchT_Syn_Proc' :: ( DifTrans ConstBranchLat ),constantPropagation_Syn_Proc' :: ( DifTrans PtConstLat ),final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set Edge ),init_Syn_Proc' :: Int,interflow_Syn_Proc' :: ( Set Inter ),pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc',strongLive_Syn_Proc' :: ( DifTrans (Set String) )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar) =
    (let ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIdStar
     in  (Syn_Proc' _lhsOconstBranchT _lhsOconstantPropagation _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive))
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
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _statOdStar :: DStar
              _statOsuccessor :: ( Maybe Int )
              _statOcontinueLabel :: ( Maybe Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Proc'
              _statIbreakLabels :: ( Set Int )
              _statIconstBranchT :: ( DifTrans ConstBranchLat )
              _statIconstantPropagation :: ( DifTrans PtConstLat )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set Edge )
              _statIinit :: Int
              _statIinterflow :: ( Set Inter )
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( DifTrans (Set String) )
              _lhsOstrongLive =
                  ({-# LINE 20 "StrongLive.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIstrongLive
                   {-# LINE 1220 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 22 "ConstantBranch.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIconstBranchT
                   {-# LINE 1225 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 21 "ConstantProp.ag" #-}
                   insertL labelEntry_ id $ insertL labelExit_ id _statIconstantPropagation
                   {-# LINE 1230 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1237 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1242 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1247 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1252 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1257 "AttributeGrammar.hs" #-}
                   )
              _statOsuccessor =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1262 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 270 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1267 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 1272 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIbreakLabels,_statIconstBranchT,_statIconstantPropagation,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
                  stat_ _statOcontinueLabel _statOdStar _statOsuccessor
          in  ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
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
                   {-# LINE 1321 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   _hdIdStar : _tlIdStar
                   {-# LINE 1326 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1335 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1340 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1345 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1362 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1367 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1376 "AttributeGrammar.hs" #-}
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
                ( ( DifTrans ConstBranchLat ),( DifTrans PtConstLat ),( Set Edge ),( Set Inter ),( [String] ),Procs',( DifTrans (Set String) ))
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: DStar}
data Syn_Procs' = Syn_Procs' {constBranchT_Syn_Procs' :: ( DifTrans ConstBranchLat ),constantPropagation_Syn_Procs' :: ( DifTrans PtConstLat ),flow_Syn_Procs' :: ( Set Edge ),interflow_Syn_Procs' :: ( Set Inter ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs',strongLive_Syn_Procs' :: ( DifTrans (Set String) )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar) =
    (let ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIdStar
     in  (Syn_Procs' _lhsOconstBranchT _lhsOconstantPropagation _lhsOflow _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Procs'
              _hdOdStar :: DStar
              _tlOdStar :: DStar
              _hdIconstBranchT :: ( DifTrans ConstBranchLat )
              _hdIconstantPropagation :: ( DifTrans PtConstLat )
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set Edge )
              _hdIinit :: Int
              _hdIinterflow :: ( Set Inter )
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _hdIstrongLive :: ( DifTrans (Set String) )
              _tlIconstBranchT :: ( DifTrans ConstBranchLat )
              _tlIconstantPropagation :: ( DifTrans PtConstLat )
              _tlIflow :: ( Set Edge )
              _tlIinterflow :: ( Set Inter )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _tlIstrongLive :: ( DifTrans (Set String) )
              _lhsOstrongLive =
                  ({-# LINE 16 "StrongLive.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1430 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 18 "ConstantBranch.ag" #-}
                   _hdIconstBranchT <> _tlIconstBranchT
                   {-# LINE 1435 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 17 "ConstantProp.ag" #-}
                   _hdIconstantPropagation <> _tlIconstantPropagation
                   {-# LINE 1440 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 256 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1445 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1450 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 258 "AttributeGrammar.ag" #-}
                   _hdIinterflow <> _tlIinterflow
                   {-# LINE 1455 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1464 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 205 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1469 "AttributeGrammar.hs" #-}
                   )
              ( _hdIconstBranchT,_hdIconstantPropagation,_hdIfinal,_hdIflow,_hdIinit,_hdIinterflow,_hdIpretty,_hdIself,_hdIstrongLive) =
                  hd_ _hdOdStar
              ( _tlIconstBranchT,_tlIconstantPropagation,_tlIflow,_tlIinterflow,_tlIpretty,_tlIself,_tlIstrongLive) =
                  tl_ _tlOdStar
          in  ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set Edge )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Procs'
              _lhsOstrongLive =
                  ({-# LINE 14 "StrongLive.ag" #-}
                   mempty
                   {-# LINE 1489 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 16 "ConstantBranch.ag" #-}
                   mempty
                   {-# LINE 1494 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 15 "ConstantProp.ag" #-}
                   mempty
                   {-# LINE 1499 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1504 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1509 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 254 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1514 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOflow,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
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
              {-# LINE 1557 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 126 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1562 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 127 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1567 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ( DifTrans ConstBranchLat ),( DifTrans PtConstLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),String,Program',( DifTrans (Set String) ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {constBranchT_Syn_Program' :: ( DifTrans ConstBranchLat ),constantPropagation_Syn_Program' :: ( DifTrans PtConstLat ),final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set Edge ),init_Syn_Program' :: Int,interflow_Syn_Program' :: ( Set Inter ),pretty_Syn_Program' :: String,self_Syn_Program' :: Program',strongLive_Syn_Program' :: ( DifTrans (Set String) )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem
     in  (Syn_Program' _lhsOconstBranchT _lhsOconstantPropagation _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         DStar ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOstrongLive :: ( DifTrans (Set String) )
         _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
         _lhsOconstantPropagation :: ( DifTrans PtConstLat )
         _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _lhsOflow :: ( Set Edge )
         _lhsOinterflow :: ( Set Inter )
         _statOdStar :: DStar
         _statOsuccessor :: ( Maybe Int )
         _procsOdStar :: DStar
         _statOcontinueLabel :: ( Maybe Int )
         _lhsOself :: Program'
         _procsIconstBranchT :: ( DifTrans ConstBranchLat )
         _procsIconstantPropagation :: ( DifTrans PtConstLat )
         _procsIflow :: ( Set Edge )
         _procsIinterflow :: ( Set Inter )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _procsIstrongLive :: ( DifTrans (Set String) )
         _statIbreakLabels :: ( Set Int )
         _statIconstBranchT :: ( DifTrans ConstBranchLat )
         _statIconstantPropagation :: ( DifTrans PtConstLat )
         _statIfinal :: ( Set Int )
         _statIflow :: ( Set Edge )
         _statIinit :: Int
         _statIinterflow :: ( Set Inter )
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIstrongLive :: ( DifTrans (Set String) )
         _lhsOstrongLive =
             ({-# LINE 10 "StrongLive.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1636 "AttributeGrammar.hs" #-}
              )
         _lhsOconstBranchT =
             ({-# LINE 12 "ConstantBranch.ag" #-}
              _procsIconstBranchT <> _statIconstBranchT
              {-# LINE 1641 "AttributeGrammar.hs" #-}
              )
         _lhsOconstantPropagation =
             ({-# LINE 11 "ConstantProp.ag" #-}
              _procsIconstantPropagation <> _statIconstantPropagation
              {-# LINE 1646 "AttributeGrammar.hs" #-}
              )
         _lhsOpretty =
             ({-# LINE 236 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1651 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 239 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1656 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 240 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1661 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1666 "AttributeGrammar.hs" #-}
              )
         _lhsOinterflow =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              _procsIinterflow <> _statIinterflow
              {-# LINE 1671 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 245 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1676 "AttributeGrammar.hs" #-}
              )
         _statOsuccessor =
             ({-# LINE 246 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1681 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 247 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1686 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 248 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1691 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         ( _procsIconstBranchT,_procsIconstantPropagation,_procsIflow,_procsIinterflow,_procsIpretty,_procsIself,_procsIstrongLive) =
             procs_ _procsOdStar
         ( _statIbreakLabels,_statIconstBranchT,_statIconstantPropagation,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
             stat_ _statOcontinueLabel _statOdStar _statOsuccessor
     in  ( _lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOpretty,_lhsOself,_lhsOstrongLive))
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
                   {-# LINE 1766 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1771 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1798 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 151 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1803 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1808 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1813 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1839 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1844 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1849 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1868 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   Print' _lhsIlabel param_
                   {-# LINE 1873 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1892 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 166 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1897 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1915 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1920 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1938 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1943 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1969 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1974 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 179 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1979 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 115 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1988 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2006 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 183 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 2011 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2028 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 187 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 2033 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2051 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 191 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2056 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2072 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2077 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2093 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2098 "AttributeGrammar.hs" #-}
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
               ( Maybe Int ) ->
               ( ( Set Int ),( DifTrans ConstBranchLat ),( DifTrans PtConstLat ),( Set Int ),( Set Edge ),Int,( Set Inter ),Bool,Bool,( [String] ),Stat',( DifTrans (Set String) ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: DStar,successor_Inh_Stat' :: ( Maybe Int )}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),constBranchT_Syn_Stat' :: ( DifTrans ConstBranchLat ),constantPropagation_Syn_Stat' :: ( DifTrans PtConstLat ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set Edge ),init_Syn_Stat' :: Int,interflow_Syn_Stat' :: ( Set Inter ),isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( DifTrans (Set String) )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar _lhsIsuccessor) =
    (let ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIcontinueLabel _lhsIdStar _lhsIsuccessor
     in  (Syn_Stat' _lhsObreakLabels _lhsOconstBranchT _lhsOconstantPropagation _lhsOfinal _lhsOflow _lhsOinit _lhsOinterflow _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
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
                   {-# LINE 2183 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 26 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2188 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 25 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2193 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2198 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2203 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2208 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2213 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2218 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 280 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2223 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2228 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2233 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
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
              _stat1Osuccessor :: ( Maybe Int )
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _stat2Osuccessor :: ( Maybe Int )
              _condIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1IbreakLabels :: ( Set Int )
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
              _stat1IconstantPropagation :: ( DifTrans PtConstLat )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set Edge )
              _stat1Iinit :: Int
              _stat1Iinterflow :: ( Set Inter )
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( DifTrans (Set String) )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
              _stat2IconstantPropagation :: ( DifTrans PtConstLat )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set Edge )
              _stat2Iinit :: Int
              _stat2Iinterflow :: ( Set Inter )
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( DifTrans (Set String) )
              _lhsOstrongLive =
                  ({-# LINE 26 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2299 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 28 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchIf (_condIconstantPropagation) _stat1Iinit _stat2Iinit) $ _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2304 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 27 "ConstantProp.ag" #-}
                   insertL labelc_ id $ _stat1IconstantPropagation <> _stat2IconstantPropagation
                   {-# LINE 2309 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2320 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2325 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2330 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2335 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 292 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2340 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 293 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2345 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2350 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2355 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _stat1OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2364 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2369 "AttributeGrammar.hs" #-}
                   )
              _stat1Osuccessor =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIsuccessor
                   {-# LINE 2374 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2379 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2384 "AttributeGrammar.hs" #-}
                   )
              _stat2Osuccessor =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIsuccessor
                   {-# LINE 2389 "AttributeGrammar.hs" #-}
                   )
              ( _condIconstantPropagation,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1IconstantPropagation,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1Osuccessor
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2IconstantPropagation,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2Osuccessor
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
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
              _statOsuccessor :: ( Maybe Int )
              _condIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIbreakLabels :: ( Set Int )
              _statIconstBranchT :: ( DifTrans ConstBranchLat )
              _statIconstantPropagation :: ( DifTrans PtConstLat )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set Edge )
              _statIinit :: Int
              _statIinterflow :: ( Set Inter )
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( DifTrans (Set String) )
              _lhsOstrongLive =
                  ({-# LINE 28 "StrongLive.ag" #-}
                   insertL labelc_ (<> _condIfreeVars) $ _statIstrongLive
                   {-# LINE 2441 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 30 "ConstantBranch.ag" #-}
                   insertL labelc_ (constBranchWhile _lhsIsuccessor (_condIconstantPropagation) _statIinit labelc_)  _statIconstBranchT
                   {-# LINE 2446 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 29 "ConstantProp.ag" #-}
                   insertL labelc_ id _statIconstantPropagation
                   {-# LINE 2451 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 295 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2458 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2463 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2468 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2473 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2478 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 302 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2483 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2488 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2493 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _statIinterflow
                   {-# LINE 2498 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2507 "AttributeGrammar.hs" #-}
                   )
              _statOsuccessor =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIsuccessor
                   {-# LINE 2512 "AttributeGrammar.hs" #-}
                   )
              ( _condIconstantPropagation,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIconstBranchT,_statIconstantPropagation,_statIfinal,_statIflow,_statIinit,_statIinterflow,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
                  stat_ _statOcontinueLabel _statOdStar _statOsuccessor
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Print' :: Int ->
                    T_Expr ->
                    (T_Stat')
sem_Stat'_Print' label_ param_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _paramIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _paramIfreeVars :: ( Set String )
              _paramIpretty :: String
              _paramIself :: Expr
              _lhsOstrongLive =
                  ({-# LINE 30 "StrongLive.ag" #-}
                   singleL label_ (<> _paramIfreeVars)
                   {-# LINE 2545 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 32 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 2550 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 31 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 2555 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   ["print(" ++ _paramIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2560 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2565 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 308 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2570 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2575 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 310 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2580 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2585 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2590 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2595 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Print' label_ _paramIself
              _lhsOself =
                  _self
              ( _paramIconstantPropagation,_paramIfreeVars,_paramIpretty,_paramIself) =
                  param_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _paramsIconstantPropagation :: ( [ConstEnv -> Maybe ConstLat] )
              _paramsIfreeVars :: ( [Set String] )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOstrongLive =
                  ({-# LINE 32 "StrongLive.ag" #-}
                   insertL labelReturn_ (aliveInExit out_ (procOut _proc    )) $ singleR labelCall_ (aliveInCall (procInp _proc    ) (procOut _proc    ) _paramsIfreeVars)
                   {-# LINE 2633 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 34 "ConstantBranch.ag" #-}
                   insertL labelCall_ (ignoreDead labelCall_ $ fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIconstantPropagation) $ singleR labelReturn_ (ignoreDead2 labelReturn_ $ ((<*>) .: fmap $ retConst out_ (procInp _proc    ) (procOut _proc    )))
                   {-# LINE 2638 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 33 "ConstantProp.ag" #-}
                   insertL labelCall_ (fmap $ callConst (procInp _proc    ) (procOut _proc    ) _paramsIconstantPropagation) $ singleR labelReturn_ (liftA2 $ retConst out_ (procInp _proc    ) (procOut _proc    ))
                   {-# LINE 2643 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2648 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2653 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 315 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2658 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2663 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2668 "AttributeGrammar.hs" #-}
                   )
              _proc =
                  ({-# LINE 318 "AttributeGrammar.ag" #-}
                   findProc name_ _lhsIdStar
                   {-# LINE 2673 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 319 "AttributeGrammar.ag" #-}
                   singleton (Inter labelCall_ (procEntry _proc    ) (procExit _proc    ) labelReturn_)
                   {-# LINE 2678 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   fromList [(labelCall_, (procEntry _proc    )), ((procExit _proc    ), labelReturn_)]
                   {-# LINE 2683 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2688 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              ( _paramsIconstantPropagation,_paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _valIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 34 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2725 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 36 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIconstantPropagation)
                   {-# LINE 2730 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 35 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIconstantPropagation)
                   {-# LINE 2735 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2740 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2745 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2750 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2755 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2760 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2765 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2770 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2775 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIconstantPropagation,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _valIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOstrongLive =
                  ({-# LINE 36 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _valIfreeVars)
                   {-# LINE 2812 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 38 "ConstantBranch.ag" #-}
                   singleL label_ (ignoreDead label_ $ fmap $ updateConst name_ _valIconstantPropagation)
                   {-# LINE 2817 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 37 "ConstantProp.ag" #-}
                   singleL label_ (fmap $ updateConst name_ _valIconstantPropagation)
                   {-# LINE 2822 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2827 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2832 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2837 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2842 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2847 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2852 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2857 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 2862 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              ( _valIconstantPropagation,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _stat1Osuccessor :: ( Maybe Int )
              _stat2Osuccessor :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: DStar
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: DStar
              _stat1IbreakLabels :: ( Set Int )
              _stat1IconstBranchT :: ( DifTrans ConstBranchLat )
              _stat1IconstantPropagation :: ( DifTrans PtConstLat )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set Edge )
              _stat1Iinit :: Int
              _stat1Iinterflow :: ( Set Inter )
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( DifTrans (Set String) )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IconstBranchT :: ( DifTrans ConstBranchLat )
              _stat2IconstantPropagation :: ( DifTrans PtConstLat )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set Edge )
              _stat2Iinit :: Int
              _stat2Iinterflow :: ( Set Inter )
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( DifTrans (Set String) )
              _lhsOstrongLive =
                  ({-# LINE 38 "StrongLive.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2923 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 40 "ConstantBranch.ag" #-}
                   _stat1IconstBranchT <> _stat2IconstBranchT
                   {-# LINE 2928 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 39 "ConstantProp.ag" #-}
                   _stat1IconstantPropagation <> _stat2IconstantPropagation
                   {-# LINE 2933 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2938 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2943 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2948 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2953 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 340 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2958 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2963 "AttributeGrammar.hs" #-}
                   )
              _stat1Osuccessor =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   Just _stat2Iinit
                   {-# LINE 2968 "AttributeGrammar.hs" #-}
                   )
              _stat2Osuccessor =
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   _lhsIsuccessor
                   {-# LINE 2973 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2978 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1Iinterflow <> _stat2Iinterflow
                   {-# LINE 2983 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _stat1OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2992 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2997 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3002 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 3007 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IconstBranchT,_stat1IconstantPropagation,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1Iinterflow,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1Osuccessor
              ( _stat2IbreakLabels,_stat2IconstBranchT,_stat2IconstantPropagation,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2Iinterflow,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2Osuccessor
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _sizeIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 40 "StrongLive.ag" #-}
                   singleL label_ (keepAlive name_ _sizeIfreeVars)
                   {-# LINE 3042 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 42 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3047 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 41 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3052 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3057 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3062 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3067 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3072 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 349 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3077 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 350 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3082 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3087 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3092 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              ( _sizeIconstantPropagation,_sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _ptrIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 42 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3128 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 44 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3133 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 43 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3138 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 3143 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3148 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3153 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3158 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3163 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 357 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3168 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3173 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3178 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              ( _ptrIconstantPropagation,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
              _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set Edge )
              _lhsObreakLabels :: ( Set Int )
              _lhsOinterflow :: ( Set Inter )
              _lhsOself :: Stat'
              _ptrIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIconstantPropagation :: ( ConstEnv -> Maybe ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOstrongLive =
                  ({-# LINE 44 "StrongLive.ag" #-}
                   singleL label_ id
                   {-# LINE 3220 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 46 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3225 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 45 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3230 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 359 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3235 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3240 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3245 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3250 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3255 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3260 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3265 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3270 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              ( _ptrIconstantPropagation,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIconstantPropagation,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
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
                   {-# LINE 3302 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 48 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3307 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 47 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3312 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 366 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3317 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 367 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3322 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3327 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3332 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 370 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3337 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   singleton (label_, (Maybe.fromMaybe (error $ "toplevel continue at " ++ show label_) _lhsIcontinueLabel))
                   {-# LINE 3342 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 210 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3347 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3352 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIsuccessor ->
         (let _lhsOstrongLive :: ( DifTrans (Set String) )
              _lhsOconstBranchT :: ( DifTrans ConstBranchLat )
              _lhsOconstantPropagation :: ( DifTrans PtConstLat )
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
                   {-# LINE 3380 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstBranchT =
                  ({-# LINE 50 "ConstantBranch.ag" #-}
                   singleL label_ (constBranchId label_)
                   {-# LINE 3385 "AttributeGrammar.hs" #-}
                   )
              _lhsOconstantPropagation =
                  ({-# LINE 49 "ConstantProp.ag" #-}
                   singleL label_ id
                   {-# LINE 3390 "AttributeGrammar.hs" #-}
                   )
              _lhsOpretty =
                  ({-# LINE 373 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3395 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3400 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 375 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3405 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 376 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3410 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 377 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3415 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 378 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3420 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 379 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3425 "AttributeGrammar.hs" #-}
                   )
              _lhsOinterflow =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   mempty
                   {-# LINE 3430 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
          in  ( _lhsObreakLabels,_lhsOconstBranchT,_lhsOconstantPropagation,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOinterflow,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))