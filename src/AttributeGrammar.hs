

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.Set
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 201 "AttributeGrammar.ag" #-}

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

survive :: String -> Set String -> Set String -> Set String
survive name freeVars strongLive = if name `member` strongLive then alive <> freeVars else alive  
  where
    alive = delete name strongLive
{-# LINE 39 "AttributeGrammar.hs" #-}

{-# LINE 376 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 46 "AttributeGrammar.hs" #-}
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
type T_BExpr = ( ( Set String ),Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {freeVars_Syn_BExpr :: ( Set String ),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
wrap_BExpr :: T_BExpr ->
              Inh_BExpr ->
              Syn_BExpr
wrap_BExpr sem (Inh_BExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_BExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_BExpr_BConst :: Bool ->
                    T_BExpr
sem_BExpr_BConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 409 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 105 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 410 "AttributeGrammar.ag" #-}
              10
              {-# LINE 110 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 115 "AttributeGrammar.hs" #-}
              )
         _self =
             BConst val_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BVar :: String ->
                  T_BExpr
sem_BExpr_BVar name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 412 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 132 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 413 "AttributeGrammar.ag" #-}
              10
              {-# LINE 137 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 414 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 142 "AttributeGrammar.hs" #-}
              )
         _self =
             BVar name_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessThan :: T_IExpr ->
                      T_IExpr ->
                      T_BExpr
sem_BExpr_LessThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 416 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 168 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 417 "AttributeGrammar.ag" #-}
              4
              {-# LINE 173 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 178 "AttributeGrammar.hs" #-}
              )
         _self =
             LessThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterThan :: T_IExpr ->
                         T_IExpr ->
                         T_BExpr
sem_BExpr_GreaterThan left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 419 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 208 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 420 "AttributeGrammar.ag" #-}
              4
              {-# LINE 213 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 218 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterThan _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_LessEqual :: T_IExpr ->
                       T_IExpr ->
                       T_BExpr
sem_BExpr_LessEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 422 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 248 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              4
              {-# LINE 253 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 258 "AttributeGrammar.hs" #-}
              )
         _self =
             LessEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_GreaterEqual :: T_IExpr ->
                          T_IExpr ->
                          T_BExpr
sem_BExpr_GreaterEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 288 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              4
              {-# LINE 293 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 298 "AttributeGrammar.hs" #-}
              )
         _self =
             GreaterEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_IEqual :: T_IExpr ->
                    T_IExpr ->
                    T_BExpr
sem_BExpr_IEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 328 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 429 "AttributeGrammar.ag" #-}
              4
              {-# LINE 333 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 338 "AttributeGrammar.hs" #-}
              )
         _self =
             IEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_BEqual :: T_BExpr ->
                    T_BExpr ->
                    T_BExpr
sem_BExpr_BEqual left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 368 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 432 "AttributeGrammar.ag" #-}
              4
              {-# LINE 373 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 378 "AttributeGrammar.hs" #-}
              )
         _self =
             BEqual _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_And :: T_BExpr ->
                 T_BExpr ->
                 T_BExpr
sem_BExpr_And left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 408 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 435 "AttributeGrammar.ag" #-}
              3
              {-# LINE 413 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 418 "AttributeGrammar.hs" #-}
              )
         _self =
             And _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Or :: T_BExpr ->
                T_BExpr ->
                T_BExpr
sem_BExpr_Or left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 437 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 448 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 438 "AttributeGrammar.ag" #-}
              2
              {-# LINE 453 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 458 "AttributeGrammar.hs" #-}
              )
         _self =
             Or _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_BExpr_Not :: T_BExpr ->
                 T_BExpr
sem_BExpr_Not val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _valIfreeVars :: ( Set String )
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 483 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 441 "AttributeGrammar.ag" #-}
              10
              {-# LINE 488 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 493 "AttributeGrammar.hs" #-}
              )
         _self =
             Not _valIself
         _lhsOself =
             _self
         ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
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
type T_Expr = ( ( Set String ),String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {freeVars_Syn_Expr :: ( Set String ),pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
wrap_Expr :: T_Expr ->
             Inh_Expr ->
             Syn_Expr
wrap_Expr sem (Inh_Expr) =
    (let ( _lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Expr _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Expr_B :: T_BExpr ->
              T_Expr
sem_Expr_B expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 445 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 536 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 541 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Expr_I :: T_IExpr ->
              T_Expr
sem_Expr_I expr_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Expr
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 447 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 563 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 568 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         ( _exprIfreeVars,_exprIprecedence,_exprIpretty,_exprIself) =
             expr_
     in  ( _lhsOfreeVars,_lhsOpretty,_lhsOself))
-- Exprs -------------------------------------------------------
type Exprs = [Expr]
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = ( ( Set String ),String,Exprs)
data Inh_Exprs = Inh_Exprs {}
data Syn_Exprs = Syn_Exprs {freeVars_Syn_Exprs :: ( Set String ),pretty_Syn_Exprs :: String,self_Syn_Exprs :: Exprs}
wrap_Exprs :: T_Exprs ->
              Inh_Exprs ->
              Syn_Exprs
wrap_Exprs sem (Inh_Exprs) =
    (let ( _lhsOfreeVars,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Exprs _lhsOfreeVars _lhsOpretty _lhsOself))
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Exprs
         _hdIfreeVars :: ( Set String )
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIfreeVars :: ( Set String )
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 610 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              _hdIfreeVars <> _tlIfreeVars
              {-# LINE 615 "AttributeGrammar.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIfreeVars,_hdIpretty,_hdIself) =
             hd_
         ( _tlIfreeVars,_tlIpretty,_tlIself) =
             tl_
     in  ( _lhsOfreeVars,_lhsOpretty,_lhsOself))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (let _lhsOpretty :: String
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: Exprs
         _lhsOpretty =
             ({-# LINE 451 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 634 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 639 "AttributeGrammar.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOpretty,_lhsOself))
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
type T_IExpr = ( ( Set String ),Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {freeVars_Syn_IExpr :: ( Set String ),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
wrap_IExpr :: T_IExpr ->
              Inh_IExpr ->
              Syn_IExpr
wrap_IExpr sem (Inh_IExpr) =
    (let ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself) = sem
     in  (Syn_IExpr _lhsOfreeVars _lhsOprecedence _lhsOpretty _lhsOself))
sem_IExpr_IConst :: Int ->
                    T_IExpr
sem_IExpr_IConst val_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 384 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 692 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 385 "AttributeGrammar.ag" #-}
              10
              {-# LINE 697 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 702 "AttributeGrammar.hs" #-}
              )
         _self =
             IConst val_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Var :: String ->
                 T_IExpr
sem_IExpr_Var name_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 387 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 719 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 388 "AttributeGrammar.ag" #-}
              10
              {-# LINE 724 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 389 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 729 "AttributeGrammar.hs" #-}
              )
         _self =
             Var name_
         _lhsOself =
             _self
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Plus :: T_IExpr ->
                  T_IExpr ->
                  T_IExpr
sem_IExpr_Plus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 391 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 755 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 392 "AttributeGrammar.ag" #-}
              6
              {-# LINE 760 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 765 "AttributeGrammar.hs" #-}
              )
         _self =
             Plus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Minus :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Minus left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 394 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 795 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 395 "AttributeGrammar.ag" #-}
              6
              {-# LINE 800 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 805 "AttributeGrammar.hs" #-}
              )
         _self =
             Minus _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Times :: T_IExpr ->
                   T_IExpr ->
                   T_IExpr
sem_IExpr_Times left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 397 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 835 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 398 "AttributeGrammar.ag" #-}
              7
              {-# LINE 840 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 845 "AttributeGrammar.hs" #-}
              )
         _self =
             Times _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Divide :: T_IExpr ->
                    T_IExpr ->
                    T_IExpr
sem_IExpr_Divide left_ right_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 400 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 875 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 401 "AttributeGrammar.ag" #-}
              7
              {-# LINE 880 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 885 "AttributeGrammar.hs" #-}
              )
         _self =
             Divide _leftIself _rightIself
         _lhsOself =
             _self
         ( _leftIfreeVars,_leftIprecedence,_leftIpretty,_leftIself) =
             left_
         ( _rightIfreeVars,_rightIprecedence,_rightIpretty,_rightIself) =
             right_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
sem_IExpr_Deref :: T_IExpr ->
                   T_IExpr
sem_IExpr_Deref ptr_ =
    (let _lhsOpretty :: String
         _lhsOprecedence :: Int
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _ptrIfreeVars :: ( Set String )
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 403 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 910 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 404 "AttributeGrammar.ag" #-}
              10
              {-# LINE 915 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 196 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 920 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOfreeVars,_lhsOprecedence,_lhsOpretty,_lhsOself))
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
                   {-# LINE 966 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 971 "AttributeGrammar.hs" #-}
                   )
              _lhsOjump =
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   (name_, (_lhsIlabel, _statIlabel))
                   {-# LINE 976 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 981 "AttributeGrammar.hs" #-}
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
               ( M.Map Int (Set String -> Set String) ) ->
               ( ( Set Int ),( Set (Int, Int) ),Int,( [String] ),Proc',( M.Map Int (Set String -> Set String) ))
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: ( [(String, (Int, Int))] ),strongLive_Inh_Proc' :: ( M.Map Int (Set String -> Set String) )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set (Int, Int) ),init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc',strongLive_Syn_Proc' :: ( M.Map Int (Set String -> Set String) )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar _lhsIstrongLive) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIdStar _lhsIstrongLive
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelExit_ name_ inp_ out_ stat_ =
    (\ _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _statOdStar :: ( [(String, (Int, Int))] )
              _statOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Proc'
              _statOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _statIbreakLabels :: ( Set Int )
              _statIcontinueLabel :: ( Maybe Int )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set (Int, Int) )
              _statIinit :: Int
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOpretty =
                  ({-# LINE 248 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1044 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 251 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1049 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 252 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1054 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 253 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1059 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 254 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1064 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1069 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _statIstrongLive
                   {-# LINE 1074 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              _statOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive
                   {-# LINE 1083 "AttributeGrammar.hs" #-}
                   )
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
                  stat_ _statOcontinueLabel _statOdStar _statOstrongLive
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
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
                   {-# LINE 1128 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 130 "AttributeGrammar.ag" #-}
                   _hdIjump : _tlIdStar
                   {-# LINE 1133 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1142 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1147 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1152 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1169 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1174 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1183 "AttributeGrammar.hs" #-}
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
                ( M.Map Int (Set String -> Set String) ) ->
                ( ( Set (Int, Int) ),( [String] ),Procs',( M.Map Int (Set String -> Set String) ))
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: ( [(String, (Int, Int))] ),strongLive_Inh_Procs' :: ( M.Map Int (Set String -> Set String) )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ( Set (Int, Int) ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs',strongLive_Syn_Procs' :: ( M.Map Int (Set String -> Set String) )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar _lhsIstrongLive) =
    (let ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIdStar _lhsIstrongLive
     in  (Syn_Procs' _lhsOflow _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Procs'
              _hdOdStar :: ( [(String, (Int, Int))] )
              _hdOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _tlOdStar :: ( [(String, (Int, Int))] )
              _tlOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set (Int, Int) )
              _hdIinit :: Int
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _hdIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _tlIflow :: ( Set (Int, Int) )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _tlIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOpretty =
                  ({-# LINE 243 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1232 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 244 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1237 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1242 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1251 "AttributeGrammar.hs" #-}
                   )
              _hdOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive
                   {-# LINE 1256 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 195 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1261 "AttributeGrammar.hs" #-}
                   )
              _tlOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _hdIstrongLive
                   {-# LINE 1266 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIpretty,_hdIself,_hdIstrongLive) =
                  hd_ _hdOdStar _hdOstrongLive
              ( _tlIflow,_tlIpretty,_tlIself,_tlIstrongLive) =
                  tl_ _tlOdStar _tlOstrongLive
          in  ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Procs'
              _lhsOpretty =
                  ({-# LINE 240 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1284 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 241 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1289 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1294 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
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
              {-# LINE 1337 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 121 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1342 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 122 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1347 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ( Set Int ),( Set (Int, Int) ),Int,String,Program',( M.Map Int (Set String -> Set String) ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set (Int, Int) ),init_Syn_Program' :: Int,pretty_Syn_Program' :: String,self_Syn_Program' :: Program',strongLive_Syn_Program' :: ( M.Map Int (Set String -> Set String) )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         ( [(String, (Int, Int))] ) ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _statOdStar :: ( [(String, (Int, Int))] )
         _statOcontinueLabel :: ( Maybe Int )
         _procsOdStar :: ( [(String, (Int, Int))] )
         _lhsOflow :: ( Set (Int, Int) )
         _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
         _lhsOself :: Program'
         _procsOstrongLive :: ( M.Map Int (Set String -> Set String) )
         _statOstrongLive :: ( M.Map Int (Set String -> Set String) )
         _procsIflow :: ( Set (Int, Int) )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _procsIstrongLive :: ( M.Map Int (Set String -> Set String) )
         _statIbreakLabels :: ( Set Int )
         _statIcontinueLabel :: ( Maybe Int )
         _statIfinal :: ( Set Int )
         _statIflow :: ( Set (Int, Int) )
         _statIinit :: Int
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _statIstrongLive :: ( M.Map Int (Set String -> Set String) )
         _lhsOpretty =
             ({-# LINE 229 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1409 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 230 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1414 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 231 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1419 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 232 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1424 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 233 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1429 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 234 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1434 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 235 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1439 "AttributeGrammar.hs" #-}
              )
         _lhsOstrongLive =
             ({-# LINE 236 "AttributeGrammar.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1444 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         _procsOstrongLive =
             ({-# LINE 199 "AttributeGrammar.ag" #-}
              error "missing rule: Program'.Program'.procs.strongLive"
              {-# LINE 1453 "AttributeGrammar.hs" #-}
              )
         _statOstrongLive =
             ({-# LINE 199 "AttributeGrammar.ag" #-}
              _procsIstrongLive
              {-# LINE 1458 "AttributeGrammar.hs" #-}
              )
         ( _procsIflow,_procsIpretty,_procsIself,_procsIstrongLive) =
             procs_ _procsOdStar _procsOstrongLive
         ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
             stat_ _statOcontinueLabel _statOdStar _statOstrongLive
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive))
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
                   {-# LINE 1526 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1531 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1558 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1563 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1568 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 148 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1573 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1599 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1604 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1609 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1630 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1635 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1653 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1658 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1676 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1681 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1707 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 169 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1712 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1717 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1726 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1744 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1749 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1766 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1771 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1789 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1794 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1810 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1815 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1831 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1836 "AttributeGrammar.hs" #-}
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
               ( [(String, (Int, Int))] ) ->
               ( M.Map Int (Set String -> Set String) ) ->
               ( ( Set Int ),( Maybe Int ),( Set Int ),( Set (Int, Int) ),Int,Bool,Bool,( [String] ),Stat',( M.Map Int (Set String -> Set String) ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: ( [(String, (Int, Int))] ),strongLive_Inh_Stat' :: ( M.Map Int (Set String -> Set String) )}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),continueLabel_Syn_Stat' :: ( Maybe Int ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set (Int, Int) ),init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( M.Map Int (Set String -> Set String) )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar _lhsIstrongLive) =
    (let ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive) = sem _lhsIcontinueLabel _lhsIdStar _lhsIstrongLive
     in  (Syn_Stat' _lhsObreakLabels _lhsOcontinueLabel _lhsOfinal _lhsOflow _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 259 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 1916 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 260 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1921 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 261 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 1926 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 1931 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 263 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 1936 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 264 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1941 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1946 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1951 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 1960 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat1OstrongLive :: ( M.Map Int (Set String -> Set String) )
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _stat2OstrongLive :: ( M.Map Int (Set String -> Set String) )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _stat1IbreakLabels :: ( Set Int )
              _stat1IcontinueLabel :: ( Maybe Int )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set (Int, Int) )
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( M.Map Int (Set String -> Set String) )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IcontinueLabel :: ( Maybe Int )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set (Int, Int) )
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOpretty =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2021 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 274 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2026 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2031 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2036 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2041 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2046 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 279 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(labelc_, (<> _condIfreeVars))]
                   {-# LINE 2051 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2056 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2065 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2070 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2075 "AttributeGrammar.hs" #-}
                   )
              _stat1OstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive
                   {-# LINE 2080 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2085 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2090 "AttributeGrammar.hs" #-}
                   )
              _stat2OstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive
                   {-# LINE 2095 "AttributeGrammar.hs" #-}
                   )
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1OstrongLive
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2OstrongLive
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _statOcontinueLabel :: ( Maybe Int )
              _statOdStar :: ( [(String, (Int, Int))] )
              _statOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _condIfreeVars :: ( Set String )
              _condIprecedence :: Int
              _condIpretty :: String
              _condIself :: BExpr
              _statIbreakLabels :: ( Set Int )
              _statIcontinueLabel :: ( Maybe Int )
              _statIfinal :: ( Set Int )
              _statIflow :: ( Set (Int, Int) )
              _statIinit :: Int
              _statIisSingle :: Bool
              _statIisSkip :: Bool
              _statIpretty :: ( [String] )
              _statIself :: Stat'
              _statIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOpretty =
                  ({-# LINE 282 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2144 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 285 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2149 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 286 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2154 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 287 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2159 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2164 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2169 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2174 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2179 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 292 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(labelc_, (<> _condIfreeVars))]
                   {-# LINE 2184 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2193 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2198 "AttributeGrammar.hs" #-}
                   )
              _statOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive
                   {-# LINE 2203 "AttributeGrammar.hs" #-}
                   )
              ( _condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive) =
                  stat_ _statOcontinueLabel _statOdStar _statOstrongLive
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramsIfreeVars :: ( Set String )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOpretty =
                  ({-# LINE 295 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2236 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 296 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2241 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 297 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2246 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 298 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2251 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2256 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   let (l_n, l_x) = Maybe.fromJust $ lookup name_ _lhsIdStar in
                   fromList [(labelCall_, l_n), (l_x, labelReturn_)]
                   {-# LINE 2262 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(labelCall_, survive out_ _paramsIfreeVars)]
                   {-# LINE 2267 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2272 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2281 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 307 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2311 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 308 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2316 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2321 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 310 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2326 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2331 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 312 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2336 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(label_, survive name_ _valIfreeVars)]
                   {-# LINE 2341 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2346 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2355 "AttributeGrammar.hs" #-}
                   )
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOpretty =
                  ({-# LINE 316 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2385 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 317 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2390 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 318 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2395 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 319 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2400 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 320 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2405 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 321 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2410 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(label_, survive name_ _valIfreeVars)]
                   {-# LINE 2415 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2420 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2429 "AttributeGrammar.hs" #-}
                   )
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat1OstrongLive :: ( M.Map Int (Set String -> Set String) )
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _stat2OstrongLive :: ( M.Map Int (Set String -> Set String) )
              _stat1IbreakLabels :: ( Set Int )
              _stat1IcontinueLabel :: ( Maybe Int )
              _stat1Ifinal :: ( Set Int )
              _stat1Iflow :: ( Set (Int, Int) )
              _stat1Iinit :: Int
              _stat1IisSingle :: Bool
              _stat1IisSkip :: Bool
              _stat1Ipretty :: ( [String] )
              _stat1Iself :: Stat'
              _stat1IstrongLive :: ( M.Map Int (Set String -> Set String) )
              _stat2IbreakLabels :: ( Set Int )
              _stat2IcontinueLabel :: ( Maybe Int )
              _stat2Ifinal :: ( Set Int )
              _stat2Iflow :: ( Set (Int, Int) )
              _stat2Iinit :: Int
              _stat2IisSingle :: Bool
              _stat2IisSkip :: Bool
              _stat2Ipretty :: ( [String] )
              _stat2Iself :: Stat'
              _stat2IstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOpretty =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2480 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2485 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2490 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2495 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2500 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 330 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2505 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2510 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2515 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2524 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2529 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2534 "AttributeGrammar.hs" #-}
                   )
              _stat1OstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive
                   {-# LINE 2539 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2544 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 194 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2549 "AttributeGrammar.hs" #-}
                   )
              _stat2OstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive
                   {-# LINE 2554 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1OstrongLive
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2OstrongLive
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2586 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2591 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2596 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2601 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2606 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2611 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   _lhsIstrongLive <> M.fromList [(label_, survive name_ _sizeIfreeVars)]
                   {-# LINE 2616 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2621 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2630 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2659 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2664 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2669 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2674 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2679 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2684 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2689 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2694 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2703 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 351 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2737 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2742 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2747 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2752 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2757 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2762 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2767 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2772 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2781 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 2807 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2812 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2817 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2822 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2827 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   singleton (label_, Maybe.fromJust _lhsIcontinueLabel)
                   {-# LINE 2832 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2837 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2842 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2851 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIstrongLive ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 368 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 2873 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2878 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 370 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2883 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2888 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2893 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 373 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2898 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2903 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2908 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2917 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive)))