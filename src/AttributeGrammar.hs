

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.Set
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 200 "AttributeGrammar.ag" #-}

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
{-# LINE 34 "AttributeGrammar.hs" #-}

{-# LINE 359 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 41 "AttributeGrammar.hs" #-}
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
type T_BExpr = ( Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 390 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 99 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 391 "AttributeGrammar.ag" #-}
              10
              {-# LINE 104 "AttributeGrammar.hs" #-}
              )
         _self =
             BConst val_
         _lhsOself =
             _self
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 393 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 120 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              10
              {-# LINE 125 "AttributeGrammar.hs" #-}
              )
         _self =
             BVar name_
         _lhsOself =
             _self
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 396 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 148 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 397 "AttributeGrammar.ag" #-}
              4
              {-# LINE 153 "AttributeGrammar.hs" #-}
              )
         _self =
             LessThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 399 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 180 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 400 "AttributeGrammar.ag" #-}
              4
              {-# LINE 185 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 402 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 212 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
              4
              {-# LINE 217 "AttributeGrammar.hs" #-}
              )
         _self =
             LessEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 405 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 244 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 406 "AttributeGrammar.ag" #-}
              4
              {-# LINE 249 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 408 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 276 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
              4
              {-# LINE 281 "AttributeGrammar.hs" #-}
              )
         _self =
             IEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 411 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 308 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              4
              {-# LINE 313 "AttributeGrammar.hs" #-}
              )
         _self =
             BEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 340 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 415 "AttributeGrammar.ag" #-}
              3
              {-# LINE 345 "AttributeGrammar.hs" #-}
              )
         _self =
             And _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 372 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 418 "AttributeGrammar.ag" #-}
              2
              {-# LINE 377 "AttributeGrammar.hs" #-}
              )
         _self =
             Or _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: BExpr
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 400 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              10
              {-# LINE 405 "AttributeGrammar.hs" #-}
              )
         _self =
             Not _valIself
         _lhsOself =
             _self
         ( _valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
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
type T_Expr = ( String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Expr _lhsOpretty _lhsOself))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _lhsOself :: Expr
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 446 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         ( _exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOpretty,_lhsOself))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _lhsOself :: Expr
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 427 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 466 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         ( _exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOpretty,_lhsOself))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( String,Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Exprs _lhsOpretty _lhsOself))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _lhsOself :: Exprs
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 433 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 505 "AttributeGrammar.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIpretty,_hdIself) =
             hd_
         ( _tlIpretty,_tlIself) =
             tl_
     in  ( _lhsOpretty,_lhsOself))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 523 "AttributeGrammar.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
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
type T_IExpr = ( Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 367 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 575 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 368 "AttributeGrammar.ag" #-}
              10
              {-# LINE 580 "AttributeGrammar.hs" #-}
              )
         _self =
             IConst val_
         _lhsOself =
             _self
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 370 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 596 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 371 "AttributeGrammar.ag" #-}
              10
              {-# LINE 601 "AttributeGrammar.hs" #-}
              )
         _self =
             Var name_
         _lhsOself =
             _self
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 373 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 624 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 374 "AttributeGrammar.ag" #-}
              6
              {-# LINE 629 "AttributeGrammar.hs" #-}
              )
         _self =
             Plus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 376 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 656 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 377 "AttributeGrammar.ag" #-}
              6
              {-# LINE 661 "AttributeGrammar.hs" #-}
              )
         _self =
             Minus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 379 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 688 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 380 "AttributeGrammar.ag" #-}
              7
              {-# LINE 693 "AttributeGrammar.hs" #-}
              )
         _self =
             Times _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 382 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 720 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 383 "AttributeGrammar.ag" #-}
              7
              {-# LINE 725 "AttributeGrammar.hs" #-}
              )
         _self =
             Divide _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOself :: IExpr
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 385 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 748 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 386 "AttributeGrammar.ag" #-}
              10
              {-# LINE 753 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         ( _ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOprecedence,_lhsOpretty,_lhsOself))
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
              ( ( (String, (Int, Int)) ),Int,Proc',Proc)
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {jump_Syn_Proc :: ( (String, (Int, Int)) ),label_Syn_Proc :: Int,labelled_Syn_Proc :: Proc',self_Syn_Proc :: Proc}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOjump,_lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Proc _lhsOjump _lhsOlabel _lhsOlabelled _lhsOself))
sem_Proc_Proc :: String ->
                 ([String]) ->
                 String ->
                 T_Stat ->
                 T_Proc
sem_Proc_Proc name_ inp_ out_ stat_ =
    (\ _lhsIlabel ->
         (let _statOlabel :: Int
              _lhsOlabelled :: Proc'
              _lhsOjump :: ( (String, (Int, Int)) )
              _lhsOlabel :: Int
              _lhsOself :: Proc
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 134 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 799 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 804 "AttributeGrammar.hs" #-}
                   )
              _lhsOjump =
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   (name_, (_lhsIlabel, _statIlabel))
                   {-# LINE 809 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 814 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOjump,_lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelExit _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelExit _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = ( [(String, (Int, Int))] ) ->
               ( ( Set Int ),( Set (Int, Int) ),Int,( [String] ),Proc')
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: ( [(String, (Int, Int))] )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set (Int, Int) ),init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc'}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself) = sem _lhsIdStar
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself))
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
              _lhsOflow :: ( Set (Int, Int) )
              _statOdStar :: ( [(String, (Int, Int))] )
              _statOcontinueLabel :: Int
              _lhsOself :: Proc'
              _statIcontinueLabel :: Int
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set (Int, Int) )
              _statIinit :: Int
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _lhsOpretty =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 871 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 876 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 245 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 881 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 246 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 886 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 247 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 891 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 248 "AttributeGrammar.ag" #-}
                   -1
                   {-# LINE 896 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself)))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Int ->
               ( ( [(String, (Int, Int))] ),Int,Procs',Procs)
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {dStar_Syn_Procs :: ( [(String, (Int, Int))] ),label_Syn_Procs :: Int,labelled_Syn_Procs :: Procs',self_Syn_Procs :: Procs}
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
              _lhsOdStar :: ( [(String, (Int, Int))] )
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIjump :: ( (String, (Int, Int)) )
              _hdIlabel :: Int
              _hdIlabelled :: Proc'
              _hdIself :: Proc
              _tlIdStar :: ( [(String, (Int, Int))] )
              _tlIlabel :: Int
              _tlIlabelled :: Procs'
              _tlIself :: Procs
              _lhsOlabelled =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 945 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 130 "AttributeGrammar.ag" #-}
                   _hdIjump : _tlIdStar
                   {-# LINE 950 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 959 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 964 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 969 "AttributeGrammar.hs" #-}
                   )
              ( _hdIjump,_hdIlabel,_hdIlabelled,_hdIself) =
                  hd_ _hdOlabel
              ( _tlIdStar,_tlIlabel,_tlIlabelled,_tlIself) =
                  tl_ _tlOlabel
          in  ( _lhsOdStar,_lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOdStar :: ( [(String, (Int, Int))] )
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _lhsOlabelled =
                  ({-# LINE 126 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 986 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 991 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1000 "AttributeGrammar.hs" #-}
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
type T_Procs' = ( [(String, (Int, Int))] ) ->
                ( ( Set (Int, Int) ),( [String] ),Procs')
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: ( [(String, (Int, Int))] )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ( Set (Int, Int) ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs'}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar) =
    (let ( _lhsOflow,_lhsOpretty,_lhsOself) = sem _lhsIdStar
     in  (Syn_Procs' _lhsOflow _lhsOpretty _lhsOself))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Procs'
              _hdOdStar :: ( [(String, (Int, Int))] )
              _tlOdStar :: ( [(String, (Int, Int))] )
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set (Int, Int) )
              _hdIinit :: Int
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _tlIflow :: ( Set (Int, Int) )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _lhsOpretty =
                  ({-# LINE 236 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1042 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 237 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1047 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1056 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1061 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIpretty,_hdIself) =
                  hd_ _hdOdStar
              ( _tlIflow,_tlIpretty,_tlIself) =
                  tl_ _tlOdStar
          in  ( _lhsOflow,_lhsOpretty,_lhsOself)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Procs'
              _lhsOpretty =
                  ({-# LINE 233 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1077 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 234 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1082 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOpretty,_lhsOself)))
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
         _procsIdStar :: ( [(String, (Int, Int))] )
         _procsIlabel :: Int
         _procsIlabelled :: Procs'
         _procsIself :: Procs
         _statIlabel :: Int
         _statIlabelled :: Stat'
         _statIself :: Stat
         _procsOlabel =
             ({-# LINE 120 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1125 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 121 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1130 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 122 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1135 "AttributeGrammar.hs" #-}
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
data Program' = Program' (Procs') (Stat') (( [(String, (Int, Int))] ))
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat _dStar) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat) _dStar)
-- semantic domain
type T_Program' = ( ( Set Int ),( Set (Int, Int) ),Int,String,Program')
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set (Int, Int) ),init_Syn_Program' :: Int,pretty_Syn_Program' :: String,self_Syn_Program' :: Program'}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         ( [(String, (Int, Int))] ) ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _statOdStar :: ( [(String, (Int, Int))] )
         _statOcontinueLabel :: Int
         _procsOdStar :: ( [(String, (Int, Int))] )
         _lhsOflow :: ( Set (Int, Int) )
         _lhsOself :: Program'
         _procsIflow :: ( Set (Int, Int) )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _statIcontinueLabel :: Int
         _statIfinal :: ( Set Int )
         _statIflow :: ( Set (Int, Int) )
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _lhsOpretty =
             ({-# LINE 223 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1191 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 224 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1196 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 225 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1201 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 226 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1206 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 227 "AttributeGrammar.ag" #-}
              -1
              {-# LINE 1211 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 228 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1216 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 229 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1221 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         ( _procsIflow,_procsIpretty,_procsIself) =
             procs_ _procsOdStar
         ( _statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself) =
             stat_ _statOcontinueLabel _statOdStar
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself))
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
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1293 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1298 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 145 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1325 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1330 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1335 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 148 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1340 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 151 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1366 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1371 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1376 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 156 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1397 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1402 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 160 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1420 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1425 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 164 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1443 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1448 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 168 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1474 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 169 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1479 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1484 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1493 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 173 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1511 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1516 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 177 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1533 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1538 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 181 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1556 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1561 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 185 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1577 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1582 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 189 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1598 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1603 "AttributeGrammar.hs" #-}
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
type T_Stat' = Int ->
               ( [(String, (Int, Int))] ) ->
               ( Int,( Set Int ),( Set (Int, Int) ),Int,Bool,Bool,( [String] ),Stat')
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: Int,dStar_Inh_Stat' :: ( [(String, (Int, Int))] )}
data Syn_Stat' = Syn_Stat' {continueLabel_Syn_Stat' :: Int,final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set (Int, Int) ),init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat'}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar) =
    (let ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself) = sem _lhsIcontinueLabel _lhsIdStar
     in  (Syn_Stat' _lhsOcontinueLabel _lhsOfinal _lhsOflow _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _lhsOpretty =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 1679 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1684 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 254 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1689 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1694 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 256 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 1699 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1704 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 1713 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _stat1OcontinueLabel :: Int
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat2OcontinueLabel :: Int
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1IcontinueLabel :: Int
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set (Int, Int) )
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat2IcontinueLabel :: Int
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set (Int, Int) )
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _lhsOpretty =
                  ({-# LINE 260 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 1764 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1769 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1774 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1779 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 270 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 1784 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 271 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 1789 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 1798 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 1803 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1808 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 1813 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1818 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOcontinueLabel :: Int
              _lhsOself :: Stat'
              _statOcontinueLabel :: Int
              _statOdStar :: ( [(String, (Int, Int))] )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIcontinueLabel :: Int
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set (Int, Int) )
              _statIinit :: Int
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _lhsOpretty =
                  ({-# LINE 274 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 1860 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1865 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1870 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1875 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 280 "AttributeGrammar.ag" #-}
                   singleton labelc_
                   {-# LINE 1880 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 1885 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 282 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 1890 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 1899 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1904 "AttributeGrammar.hs" #-}
                   )
              ( _condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself) =
                  stat_ _statOcontinueLabel _statOdStar
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOpretty =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 1933 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 286 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1938 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 287 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1943 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 1948 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 1953 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   let (l_n, l_x) = Maybe.fromJust $ lookup name_ _lhsIdStar in
                   fromList [(labelCall_, l_n), (l_x, labelReturn_)]
                   {-# LINE 1959 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 1968 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 296 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 1994 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 1999 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2004 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2009 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2014 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2019 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2028 "AttributeGrammar.hs" #-}
                   )
              ( _valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOpretty =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2054 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 305 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2059 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2064 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2069 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 308 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2074 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2079 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2088 "AttributeGrammar.hs" #-}
                   )
              ( _valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _stat1OcontinueLabel :: Int
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat2OcontinueLabel :: Int
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _stat1IcontinueLabel :: Int
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set (Int, Int) )
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat2IcontinueLabel :: Int
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set (Int, Int) )
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _lhsOpretty =
                  ({-# LINE 312 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2130 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2135 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2140 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 315 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2145 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2150 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2155 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2164 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2169 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2174 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2179 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2184 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar
              ( _stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2212 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2217 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2222 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2227 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2232 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2237 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2246 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2271 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2276 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2281 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 331 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2286 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2291 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2296 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2305 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2334 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2339 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2344 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2349 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 340 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2354 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 341 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2359 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2368 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _lhsOpretty =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2391 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2396 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2401 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2406 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 348 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2411 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 349 "AttributeGrammar.ag" #-}
                   singleton (label_, _lhsIcontinueLabel)
                   {-# LINE 2416 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2425 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))
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
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: Int
              _lhsOpretty =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2444 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2449 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2454 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2459 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2464 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 357 "AttributeGrammar.ag" #-}
                   singleton (label_, _lhsIcontinueLabel)
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2478 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself)))