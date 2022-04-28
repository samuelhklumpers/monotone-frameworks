

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
{-# LINE 11 "AttributeGrammar.hs" #-}

{-# LINE 175 "AttributeGrammar.ag" #-}

indent :: [String] -> [String]
indent = map ("  " ++)

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
{-# LINE 33 "AttributeGrammar.hs" #-}

{-# LINE 270 "AttributeGrammar.ag" #-}

parensIf :: Bool -> String -> String
parensIf False str = str
parensIf True str = "(" ++ str ++ ")"
{-# LINE 40 "AttributeGrammar.hs" #-}
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
             ({-# LINE 301 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 98 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 302 "AttributeGrammar.ag" #-}
              10
              {-# LINE 103 "AttributeGrammar.hs" #-}
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
             ({-# LINE 304 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 119 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 305 "AttributeGrammar.ag" #-}
              10
              {-# LINE 124 "AttributeGrammar.hs" #-}
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
             ({-# LINE 307 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 147 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 308 "AttributeGrammar.ag" #-}
              4
              {-# LINE 152 "AttributeGrammar.hs" #-}
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
             ({-# LINE 310 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 179 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 311 "AttributeGrammar.ag" #-}
              4
              {-# LINE 184 "AttributeGrammar.hs" #-}
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
             ({-# LINE 313 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 211 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 314 "AttributeGrammar.ag" #-}
              4
              {-# LINE 216 "AttributeGrammar.hs" #-}
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
             ({-# LINE 316 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 243 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 317 "AttributeGrammar.ag" #-}
              4
              {-# LINE 248 "AttributeGrammar.hs" #-}
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
             ({-# LINE 319 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 275 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 320 "AttributeGrammar.ag" #-}
              4
              {-# LINE 280 "AttributeGrammar.hs" #-}
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
             ({-# LINE 322 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 307 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 323 "AttributeGrammar.ag" #-}
              4
              {-# LINE 312 "AttributeGrammar.hs" #-}
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
             ({-# LINE 325 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 339 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 326 "AttributeGrammar.ag" #-}
              3
              {-# LINE 344 "AttributeGrammar.hs" #-}
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
             ({-# LINE 328 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 371 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 329 "AttributeGrammar.ag" #-}
              2
              {-# LINE 376 "AttributeGrammar.hs" #-}
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
             ({-# LINE 331 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 399 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 332 "AttributeGrammar.ag" #-}
              10
              {-# LINE 404 "AttributeGrammar.hs" #-}
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
             ({-# LINE 336 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 445 "AttributeGrammar.hs" #-}
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
             ({-# LINE 338 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 465 "AttributeGrammar.hs" #-}
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
             ({-# LINE 344 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 504 "AttributeGrammar.hs" #-}
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
             ({-# LINE 342 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 522 "AttributeGrammar.hs" #-}
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
             ({-# LINE 278 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 574 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 279 "AttributeGrammar.ag" #-}
              10
              {-# LINE 579 "AttributeGrammar.hs" #-}
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
             ({-# LINE 281 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 595 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 282 "AttributeGrammar.ag" #-}
              10
              {-# LINE 600 "AttributeGrammar.hs" #-}
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
             ({-# LINE 284 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 623 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 285 "AttributeGrammar.ag" #-}
              6
              {-# LINE 628 "AttributeGrammar.hs" #-}
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
             ({-# LINE 287 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 655 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 288 "AttributeGrammar.ag" #-}
              6
              {-# LINE 660 "AttributeGrammar.hs" #-}
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
             ({-# LINE 290 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 687 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 291 "AttributeGrammar.ag" #-}
              7
              {-# LINE 692 "AttributeGrammar.hs" #-}
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
             ({-# LINE 293 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 719 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 294 "AttributeGrammar.ag" #-}
              7
              {-# LINE 724 "AttributeGrammar.hs" #-}
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
             ({-# LINE 296 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 747 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 297 "AttributeGrammar.ag" #-}
              10
              {-# LINE 752 "AttributeGrammar.hs" #-}
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
              ( Int,Proc',Proc)
data Inh_Proc = Inh_Proc {label_Inh_Proc :: Int}
data Syn_Proc = Syn_Proc {label_Syn_Proc :: Int,labelled_Syn_Proc :: Proc',self_Syn_Proc :: Proc}
wrap_Proc :: T_Proc ->
             Inh_Proc ->
             Syn_Proc
wrap_Proc sem (Inh_Proc _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Proc _lhsOlabel _lhsOlabelled _lhsOself))
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
              _lhsOself :: Proc
              _statIlabel :: Int
              _statIlabelled :: Stat'
              _statIself :: Stat
              _statOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 797 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 112 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 802 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 113 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 807 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              ( _statIlabel,_statIlabelled,_statIself) =
                  stat_ _statOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Proc' -------------------------------------------------------
data Proc' = Proc' (Int) (Int) (String) (([String])) (String) (Stat')
           deriving ( Show)
-- cata
sem_Proc' :: (Proc') ->
             (T_Proc')
sem_Proc' (Proc' _labelEntry _labelExit _name _inp _out _stat) =
    (sem_Proc'_Proc' _labelEntry _labelExit _name _inp _out (sem_Stat' _stat))
-- semantic domain
type T_Proc' = ( ( [String] ),Proc')
data Inh_Proc' = Inh_Proc' {}
data Syn_Proc' = Syn_Proc' {pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc'}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc') =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Proc' _lhsOpretty _lhsOself))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelExit_ name_ inp_ out_ stat_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOself :: Proc'
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _lhsOpretty =
             ({-# LINE 208 "AttributeGrammar.ag" #-}
              ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
               ++ indent _statIpretty
               ++ ["end" ++ showLabel labelExit_ ++ ";"]
              {-# LINE 853 "AttributeGrammar.hs" #-}
              )
         _self =
             Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
         _lhsOself =
             _self
         ( _statIisSingle,_statIisSkip,_statIpretty,_statIself) =
             stat_
     in  ( _lhsOpretty,_lhsOself))
-- Procs -------------------------------------------------------
type Procs = [Proc]
-- cata
sem_Procs :: Procs ->
             T_Procs
sem_Procs list =
    (Prelude.foldr sem_Procs_Cons sem_Procs_Nil (Prelude.map sem_Proc list))
-- semantic domain
type T_Procs = Int ->
               ( Int,Procs',Procs)
data Inh_Procs = Inh_Procs {label_Inh_Procs :: Int}
data Syn_Procs = Syn_Procs {label_Syn_Procs :: Int,labelled_Syn_Procs :: Procs',self_Syn_Procs :: Procs}
wrap_Procs :: T_Procs ->
              Inh_Procs ->
              Syn_Procs
wrap_Procs sem (Inh_Procs _lhsIlabel) =
    (let ( _lhsOlabel,_lhsOlabelled,_lhsOself) = sem _lhsIlabel
     in  (Syn_Procs _lhsOlabel _lhsOlabelled _lhsOself))
sem_Procs_Cons :: T_Proc ->
                  T_Procs ->
                  T_Procs
sem_Procs_Cons hd_ tl_ =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _hdOlabel :: Int
              _tlOlabel :: Int
              _hdIlabel :: Int
              _hdIlabelled :: Proc'
              _hdIself :: Proc
              _tlIlabel :: Int
              _tlIlabelled :: Procs'
              _tlIself :: Procs
              _lhsOlabelled =
                  ({-# LINE 107 "AttributeGrammar.ag" #-}
                   _hdIlabelled : _tlIlabelled
                   {-# LINE 899 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 90 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 908 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 90 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 913 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 90 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 918 "AttributeGrammar.hs" #-}
                   )
              ( _hdIlabel,_hdIlabelled,_hdIself) =
                  hd_ _hdOlabel
              ( _tlIlabel,_tlIlabelled,_tlIself) =
                  tl_ _tlOlabel
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
sem_Procs_Nil :: T_Procs
sem_Procs_Nil =
    (\ _lhsIlabel ->
         (let _lhsOlabelled :: Procs'
              _lhsOself :: Procs
              _lhsOlabel :: Int
              _lhsOlabelled =
                  ({-# LINE 105 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 934 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 90 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 943 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsOlabel,_lhsOlabelled,_lhsOself)))
-- Procs' ------------------------------------------------------
type Procs' = [Proc']
-- cata
sem_Procs' :: (Procs') ->
              (T_Procs')
sem_Procs' list =
    (Prelude.foldr sem_Procs'_Cons sem_Procs'_Nil (Prelude.map sem_Proc' list))
-- semantic domain
type T_Procs' = ( ( [String] ),Procs')
data Inh_Procs' = Inh_Procs' {}
data Syn_Procs' = Syn_Procs' {pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs'}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs') =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Procs' _lhsOpretty _lhsOself))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOself :: Procs'
         _hdIpretty :: ( [String] )
         _hdIself :: Proc'
         _tlIpretty :: ( [String] )
         _tlIself :: Procs'
         _lhsOpretty =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              _hdIpretty ++ _tlIpretty
              {-# LINE 976 "AttributeGrammar.hs" #-}
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
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (let _lhsOpretty :: ( [String] )
         _lhsOself :: Procs'
         _lhsOpretty =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              []
              {-# LINE 994 "AttributeGrammar.hs" #-}
              )
         _self =
             []
         _lhsOself =
             _self
     in  ( _lhsOpretty,_lhsOself))
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
         _procsIlabel :: Int
         _procsIlabelled :: Procs'
         _procsIself :: Procs
         _statIlabel :: Int
         _statIlabelled :: Stat'
         _statIself :: Stat
         _procsOlabel =
             ({-# LINE 99 "AttributeGrammar.ag" #-}
              1
              {-# LINE 1036 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 100 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1041 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 101 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled
              {-# LINE 1046 "AttributeGrammar.hs" #-}
              )
         _self =
             Program _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIlabel,_procsIlabelled,_procsIself) =
             procs_ _procsOlabel
         ( _statIlabel,_statIlabelled,_statIself) =
             stat_ _statOlabel
     in  ( _lhsOlabelled,_lhsOself))
-- Program' ----------------------------------------------------
data Program' = Program' (Procs') (Stat')
              deriving ( Show)
-- cata
sem_Program' :: (Program') ->
                (T_Program')
sem_Program' (Program' _procs _stat) =
    (sem_Program'_Program' (sem_Procs' _procs) (sem_Stat' _stat))
-- semantic domain
type T_Program' = ( String,Program')
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {pretty_Syn_Program' :: String,self_Syn_Program' :: Program'}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOpretty,_lhsOself) = sem
     in  (Syn_Program' _lhsOpretty _lhsOself))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ =
    (let _lhsOpretty :: String
         _lhsOself :: Program'
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _lhsOpretty =
             ({-# LINE 198 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1090 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself
         _lhsOself =
             _self
         ( _procsIpretty,_procsIself) =
             procs_
         ( _statIisSingle,_statIisSkip,_statIpretty,_statIself) =
             stat_
     in  ( _lhsOpretty,_lhsOself))
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
                  ({-# LINE 117 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1162 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 118 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1167 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 121 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1194 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 122 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1199 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 123 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1204 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 124 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1209 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1235 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 128 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1240 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 129 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1245 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 132 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 2
                   {-# LINE 1266 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 133 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1271 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1289 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1294 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 140 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1312 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 141 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1317 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 144 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1343 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 145 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1348 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1353 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 90 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1362 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 149 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1380 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 150 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1385 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1402 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 154 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1407 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1425 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 158 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 1430 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1446 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 162 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 1451 "AttributeGrammar.hs" #-}
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
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   _lhsIlabel + 1
                   {-# LINE 1467 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 166 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 1472 "AttributeGrammar.hs" #-}
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
type T_Stat' = ( Bool,Bool,( [String] ),Stat')
data Inh_Stat' = Inh_Stat' {}
data Syn_Stat' = Syn_Stat' {isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat'}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat') =
    (let ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself) = sem
     in  (Syn_Stat' _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _lhsOpretty =
             ({-# LINE 214 "AttributeGrammar.ag" #-}
              ["skip" ++ showLabel label_]
              {-# LINE 1540 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 215 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1545 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 216 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1550 "AttributeGrammar.hs" #-}
              )
         _self =
             Skip' label_
         _lhsOself =
             _self
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _condIprecedence :: Int
         _condIpretty :: String
         _condIself :: BExpr
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat1Iself :: Stat'
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _stat2Iself :: Stat'
         _lhsOpretty =
             ({-# LINE 218 "AttributeGrammar.ag" #-}
              ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
               ++ indent _stat1Ipretty
               ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                       [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                   ++ indent _stat2Ipretty
                   ++ (if _stat2IisSingle then [] else ["}"])
                 )
              {-# LINE 1587 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 225 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1592 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 226 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1597 "AttributeGrammar.hs" #-}
              )
         _self =
             IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
         _lhsOself =
             _self
         ( _condIprecedence,_condIpretty,_condIself) =
             cond_
         ( _stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself) =
             stat1_
         ( _stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself) =
             stat2_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _condIprecedence :: Int
         _condIpretty :: String
         _condIself :: BExpr
         _statIisSingle :: Bool
         _statIisSkip :: Bool
         _statIpretty :: ( [String] )
         _statIself :: Stat'
         _lhsOpretty =
             ({-# LINE 228 "AttributeGrammar.ag" #-}
              ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
               ++ indent _statIpretty
               ++ (if _statIisSingle then [] else ["}"])
              {-# LINE 1631 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 231 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1636 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 232 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1641 "AttributeGrammar.hs" #-}
              )
         _self =
             While' labelc_ _condIself _statIself
         _lhsOself =
             _self
         ( _condIprecedence,_condIpretty,_condIself) =
             cond_
         ( _statIisSingle,_statIisSkip,_statIpretty,_statIself) =
             stat_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _paramsIpretty :: String
         _paramsIself :: Exprs
         _lhsOpretty =
             ({-# LINE 234 "AttributeGrammar.ag" #-}
              ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
              {-# LINE 1668 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 235 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1673 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 236 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1678 "AttributeGrammar.hs" #-}
              )
         _self =
             Call' labelCall_ labelReturn_ name_ _paramsIself out_
         _lhsOself =
             _self
         ( _paramsIpretty,_paramsIself) =
             params_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: IExpr
         _lhsOpretty =
             ({-# LINE 238 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1702 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 239 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1707 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 240 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1712 "AttributeGrammar.hs" #-}
              )
         _self =
             IAssign' label_ name_ _valIself
         _lhsOself =
             _self
         ( _valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1736 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 243 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1741 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 244 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1746 "AttributeGrammar.hs" #-}
              )
         _self =
             BAssign' label_ name_ _valIself
         _lhsOself =
             _self
         ( _valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _stat1IisSingle :: Bool
         _stat1IisSkip :: Bool
         _stat1Ipretty :: ( [String] )
         _stat1Iself :: Stat'
         _stat2IisSingle :: Bool
         _stat2IisSkip :: Bool
         _stat2Ipretty :: ( [String] )
         _stat2Iself :: Stat'
         _lhsOpretty =
             ({-# LINE 246 "AttributeGrammar.ag" #-}
              addSemicolon _stat1Ipretty ++ _stat2Ipretty
              {-# LINE 1774 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 247 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1779 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 248 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1784 "AttributeGrammar.hs" #-}
              )
         _self =
             Seq' _stat1Iself _stat2Iself
         _lhsOself =
             _self
         ( _stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself) =
             stat1_
         ( _stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself) =
             stat2_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _sizeIprecedence :: Int
         _sizeIpretty :: String
         _sizeIself :: IExpr
         _lhsOpretty =
             ({-# LINE 250 "AttributeGrammar.ag" #-}
              ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1810 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 251 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1815 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 252 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1820 "AttributeGrammar.hs" #-}
              )
         _self =
             Malloc' label_ name_ _sizeIself
         _lhsOself =
             _self
         ( _sizeIprecedence,_sizeIpretty,_sizeIself) =
             size_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 254 "AttributeGrammar.ag" #-}
              ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
              {-# LINE 1843 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 255 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1848 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 256 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1853 "AttributeGrammar.hs" #-}
              )
         _self =
             Free' label_ _ptrIself
         _lhsOself =
             _self
         ( _ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: IExpr
         _lhsOpretty =
             ({-# LINE 258 "AttributeGrammar.ag" #-}
              ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
              {-# LINE 1880 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 259 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1885 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 260 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1890 "AttributeGrammar.hs" #-}
              )
         _self =
             RefAssign' label_ _ptrIself _valIself
         _lhsOself =
             _self
         ( _ptrIprecedence,_ptrIpretty,_ptrIself) =
             ptr_
         ( _valIprecedence,_valIpretty,_valIself) =
             val_
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _lhsOpretty =
             ({-# LINE 262 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1911 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 263 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1916 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 264 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1921 "AttributeGrammar.hs" #-}
              )
         _self =
             Continue' label_
         _lhsOself =
             _self
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (let _lhsOpretty :: ( [String] )
         _lhsOisSkip :: Bool
         _lhsOisSingle :: Bool
         _lhsOself :: Stat'
         _lhsOpretty =
             ({-# LINE 266 "AttributeGrammar.ag" #-}
              ["continue" ++ showLabel label_]
              {-# LINE 1938 "AttributeGrammar.hs" #-}
              )
         _lhsOisSkip =
             ({-# LINE 267 "AttributeGrammar.ag" #-}
              False
              {-# LINE 1943 "AttributeGrammar.hs" #-}
              )
         _lhsOisSingle =
             ({-# LINE 268 "AttributeGrammar.ag" #-}
              True
              {-# LINE 1948 "AttributeGrammar.hs" #-}
              )
         _self =
             Break' label_
         _lhsOself =
             _self
     in  ( _lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself))