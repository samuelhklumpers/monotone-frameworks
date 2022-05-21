

-- UUAGC 0.9.53.1 (AttributeGrammar)
module AttributeGrammar where
{-# LINE 1 "AttributeGrammar.ag" #-}

import qualified Data.Map as M
import qualified Data.Maybe as Maybe
import qualified Data.List as L
import Data.Set
{-# LINE 12 "AttributeGrammar.hs" #-}

{-# LINE 192 "AttributeGrammar.ag" #-}

data ConstLat = CI Int | CB Bool | NonConst deriving Eq
{-# LINE 17 "AttributeGrammar.hs" #-}

{-# LINE 206 "AttributeGrammar.ag" #-}

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

testConstant :: String -> ConstLat -> M.Map String ConstLat -> M.Map String ConstLat
testConstant name val env = M.insert name val' env
  where
    val' = case M.lookup name env of
      Just y  -> if val == y then y else NonConst
      Nothing -> val
{-# LINE 51 "AttributeGrammar.hs" #-}

{-# LINE 394 "AttributeGrammar.ag" #-}

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
{-# LINE 77 "AttributeGrammar.hs" #-}
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
type T_BExpr = ( ( M.Map String ConstLat -> ConstLat ),( Set String ),Int,String,BExpr)
data Inh_BExpr = Inh_BExpr {}
data Syn_BExpr = Syn_BExpr {expValSpace_Syn_BExpr :: ( M.Map String ConstLat -> ConstLat ),freeVars_Syn_BExpr :: ( Set String ),precedence_Syn_BExpr :: Int,pretty_Syn_BExpr :: String,self_Syn_BExpr :: BExpr}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 453 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 137 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 454 "AttributeGrammar.ag" #-}
              10
              {-# LINE 142 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 455 "AttributeGrammar.ag" #-}
              \_ -> CB val_
              {-# LINE 147 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 152 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOself :: BExpr
         _lhsOpretty =
             ({-# LINE 457 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 170 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 458 "AttributeGrammar.ag" #-}
              10
              {-# LINE 175 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 459 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 180 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 460 "AttributeGrammar.ag" #-}
              (M.! name_)
              {-# LINE 185 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 462 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " < " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 214 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 463 "AttributeGrammar.ag" #-}
              4
              {-# LINE 219 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 464 "AttributeGrammar.ag" #-}
              \env -> cIIB (<) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 224 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 229 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 466 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " > " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 262 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 467 "AttributeGrammar.ag" #-}
              4
              {-# LINE 267 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 468 "AttributeGrammar.ag" #-}
              \env -> cIIB (>) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 272 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 277 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 470 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " <= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 310 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 471 "AttributeGrammar.ag" #-}
              4
              {-# LINE 315 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 472 "AttributeGrammar.ag" #-}
              \env -> cIIB (<=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 320 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 325 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 474 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " >= " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 358 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 475 "AttributeGrammar.ag" #-}
              4
              {-# LINE 363 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 476 "AttributeGrammar.ag" #-}
              \env -> cIIB (>=) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 368 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 373 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 478 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 406 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 479 "AttributeGrammar.ag" #-}
              4
              {-# LINE 411 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 480 "AttributeGrammar.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 416 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 421 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 482 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence <= 4) _leftIpretty ++ " == " ++ parensIf (_rightIprecedence <= 4) _rightIpretty
              {-# LINE 454 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 483 "AttributeGrammar.ag" #-}
              4
              {-# LINE 459 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 484 "AttributeGrammar.ag" #-}
              \env -> cIIB (==) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 464 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 469 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 486 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 3) _leftIpretty ++ " && " ++ parensIf (_rightIprecedence < 3) _rightIpretty
              {-# LINE 502 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 487 "AttributeGrammar.ag" #-}
              3
              {-# LINE 507 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 488 "AttributeGrammar.ag" #-}
              \env -> cBBB (&&) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 512 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 517 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: BExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: BExpr
         _lhsOpretty =
             ({-# LINE 490 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 2) _leftIpretty ++ " || " ++ parensIf (_rightIprecedence < 2) _rightIpretty
              {-# LINE 550 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 491 "AttributeGrammar.ag" #-}
              2
              {-# LINE 555 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 492 "AttributeGrammar.ag" #-}
              \env -> cBBB (||) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 560 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 565 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: BExpr
         _valIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _valIfreeVars :: ( Set String )
         _valIprecedence :: Int
         _valIpretty :: String
         _valIself :: BExpr
         _lhsOpretty =
             ({-# LINE 494 "AttributeGrammar.ag" #-}
              "not " ++ parensIf (_valIprecedence < 10) _valIpretty
              {-# LINE 592 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 495 "AttributeGrammar.ag" #-}
              10
              {-# LINE 597 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 496 "AttributeGrammar.ag" #-}
              \env -> cBB not (_valIexpValSpace env)
              {-# LINE 602 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _valIfreeVars
              {-# LINE 607 "AttributeGrammar.hs" #-}
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
type T_Expr = ( ( M.Map String ConstLat -> ConstLat ),( Set String ),String,Expr)
data Inh_Expr = Inh_Expr {}
data Syn_Expr = Syn_Expr {expValSpace_Syn_Expr :: ( M.Map String ConstLat -> ConstLat ),freeVars_Syn_Expr :: ( Set String ),pretty_Syn_Expr :: String,self_Syn_Expr :: Expr}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _exprIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: BExpr
         _lhsOpretty =
             ({-# LINE 500 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 652 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 657 "AttributeGrammar.hs" #-}
              )
         _self =
             B _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 201 "AttributeGrammar.ag" #-}
              _exprIexpValSpace
              {-# LINE 666 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _exprIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _exprIfreeVars :: ( Set String )
         _exprIprecedence :: Int
         _exprIpretty :: String
         _exprIself :: IExpr
         _lhsOpretty =
             ({-# LINE 502 "AttributeGrammar.ag" #-}
              _exprIpretty
              {-# LINE 686 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _exprIfreeVars
              {-# LINE 691 "AttributeGrammar.hs" #-}
              )
         _self =
             I _exprIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 201 "AttributeGrammar.ag" #-}
              _exprIexpValSpace
              {-# LINE 700 "AttributeGrammar.hs" #-}
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
         _hdIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _hdIfreeVars :: ( Set String )
         _hdIpretty :: String
         _hdIself :: Expr
         _tlIfreeVars :: ( Set String )
         _tlIpretty :: String
         _tlIself :: Exprs
         _lhsOpretty =
             ({-# LINE 508 "AttributeGrammar.ag" #-}
              _hdIpretty ++ ", " ++ _tlIpretty
              {-# LINE 739 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              _hdIfreeVars <> _tlIfreeVars
              {-# LINE 744 "AttributeGrammar.hs" #-}
              )
         _self =
             (:) _hdIself _tlIself
         _lhsOself =
             _self
         ( _hdIexpValSpace,_hdIfreeVars,_hdIpretty,_hdIself) =
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
             ({-# LINE 506 "AttributeGrammar.ag" #-}
              ""
              {-# LINE 763 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 202 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 768 "AttributeGrammar.hs" #-}
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
type T_IExpr = ( ( M.Map String ConstLat -> ConstLat ),( Set String ),Int,String,IExpr)
data Inh_IExpr = Inh_IExpr {}
data Syn_IExpr = Syn_IExpr {expValSpace_Syn_IExpr :: ( M.Map String ConstLat -> ConstLat ),freeVars_Syn_IExpr :: ( Set String ),precedence_Syn_IExpr :: Int,pretty_Syn_IExpr :: String,self_Syn_IExpr :: IExpr}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 421 "AttributeGrammar.ag" #-}
              show val_
              {-# LINE 822 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 422 "AttributeGrammar.ag" #-}
              10
              {-# LINE 827 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 423 "AttributeGrammar.ag" #-}
              \_ -> CI val_
              {-# LINE 832 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              empty
              {-# LINE 837 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOself :: IExpr
         _lhsOpretty =
             ({-# LINE 425 "AttributeGrammar.ag" #-}
              name_
              {-# LINE 855 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 426 "AttributeGrammar.ag" #-}
              10
              {-# LINE 860 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 427 "AttributeGrammar.ag" #-}
              singleton name_
              {-# LINE 865 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 428 "AttributeGrammar.ag" #-}
              (M.! name_)
              {-# LINE 870 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 430 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " + " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 899 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 431 "AttributeGrammar.ag" #-}
              6
              {-# LINE 904 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 432 "AttributeGrammar.ag" #-}
              \env -> cIII (+) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 909 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 914 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 434 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 6) _leftIpretty ++ " - " ++ parensIf (_rightIprecedence <= 6) _rightIpretty
              {-# LINE 947 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 435 "AttributeGrammar.ag" #-}
              6
              {-# LINE 952 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 436 "AttributeGrammar.ag" #-}
              \env -> cIII (-) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 957 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 962 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 438 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " * " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 995 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 439 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1000 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 440 "AttributeGrammar.ag" #-}
              \env -> cIII (*) (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1005 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1010 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _lhsOfreeVars :: ( Set String )
         _lhsOself :: IExpr
         _leftIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _leftIfreeVars :: ( Set String )
         _leftIprecedence :: Int
         _leftIpretty :: String
         _leftIself :: IExpr
         _rightIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _rightIfreeVars :: ( Set String )
         _rightIprecedence :: Int
         _rightIpretty :: String
         _rightIself :: IExpr
         _lhsOpretty =
             ({-# LINE 442 "AttributeGrammar.ag" #-}
              parensIf (_leftIprecedence < 7) _leftIpretty ++ " / " ++ parensIf (_rightIprecedence <= 7) _rightIpretty
              {-# LINE 1043 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 443 "AttributeGrammar.ag" #-}
              7
              {-# LINE 1048 "AttributeGrammar.hs" #-}
              )
         _lhsOexpValSpace =
             ({-# LINE 444 "AttributeGrammar.ag" #-}
              \env -> cIII div (_leftIexpValSpace env) (_rightIexpValSpace env)
              {-# LINE 1053 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _leftIfreeVars <> _rightIfreeVars
              {-# LINE 1058 "AttributeGrammar.hs" #-}
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
         _lhsOexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _ptrIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
         _ptrIfreeVars :: ( Set String )
         _ptrIprecedence :: Int
         _ptrIpretty :: String
         _ptrIself :: IExpr
         _lhsOpretty =
             ({-# LINE 446 "AttributeGrammar.ag" #-}
              "*" ++ parensIf (_ptrIprecedence < 10) _ptrIpretty
              {-# LINE 1085 "AttributeGrammar.hs" #-}
              )
         _lhsOprecedence =
             ({-# LINE 447 "AttributeGrammar.ag" #-}
              10
              {-# LINE 1090 "AttributeGrammar.hs" #-}
              )
         _lhsOfreeVars =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _ptrIfreeVars
              {-# LINE 1095 "AttributeGrammar.hs" #-}
              )
         _self =
             Deref _ptrIself
         _lhsOself =
             _self
         _lhsOexpValSpace =
             ({-# LINE 200 "AttributeGrammar.ag" #-}
              _ptrIexpValSpace
              {-# LINE 1104 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1146 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 135 "AttributeGrammar.ag" #-}
                   Proc' _lhsIlabel _statIlabel name_ inp_ out_ _statIlabelled
                   {-# LINE 1151 "AttributeGrammar.hs" #-}
                   )
              _lhsOjump =
                  ({-# LINE 136 "AttributeGrammar.ag" #-}
                   (name_, (_lhsIlabel, _statIlabel))
                   {-# LINE 1156 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 137 "AttributeGrammar.ag" #-}
                   _statIlabel + 1
                   {-# LINE 1161 "AttributeGrammar.hs" #-}
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
               ( M.Map Int (M.Map String ConstLat) ) ->
               ( ( Set Int ),( Set (Int, Int) ),Int,( [String] ),Proc',( M.Map Int (Set String -> Set String) ),( M.Map Int (M.Map String ConstLat) ))
data Inh_Proc' = Inh_Proc' {dStar_Inh_Proc' :: ( [(String, (Int, Int))] ),valSpace_Inh_Proc' :: ( M.Map Int (M.Map String ConstLat) )}
data Syn_Proc' = Syn_Proc' {final_Syn_Proc' :: ( Set Int ),flow_Syn_Proc' :: ( Set (Int, Int) ),init_Syn_Proc' :: Int,pretty_Syn_Proc' :: ( [String] ),self_Syn_Proc' :: Proc',strongLive_Syn_Proc' :: ( M.Map Int (Set String -> Set String) ),valSpace_Syn_Proc' :: ( M.Map Int (M.Map String ConstLat) )}
wrap_Proc' :: (T_Proc') ->
              (Inh_Proc') ->
              (Syn_Proc')
wrap_Proc' sem (Inh_Proc' _lhsIdStar _lhsIvalSpace) =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar _lhsIvalSpace
     in  (Syn_Proc' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Proc'_Proc' :: Int ->
                   Int ->
                   String ->
                   ([String]) ->
                   String ->
                   (T_Stat') ->
                   (T_Proc')
sem_Proc'_Proc' labelEntry_ labelExit_ name_ inp_ out_ stat_ =
    (\ _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _statOdStar :: ( [(String, (Int, Int))] )
              _statOcontinueLabel :: ( Maybe Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Proc'
              _statOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
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
              _statIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOpretty =
                  ({-# LINE 262 "AttributeGrammar.ag" #-}
                   ["proc " ++ name_ ++ "(val " ++ (inp_ >>= (++ ", ")) ++ "out " ++ out_ ++ ") is" ++ showLabel labelEntry_]
                    ++ indent _statIpretty
                    ++ ["end" ++ showLabel labelExit_ ++ ";"]
                   {-# LINE 1226 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 265 "AttributeGrammar.ag" #-}
                   _statIinit
                   {-# LINE 1231 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 266 "AttributeGrammar.ag" #-}
                   _statIfinal
                   {-# LINE 1236 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 267 "AttributeGrammar.ag" #-}
                   singleton (labelEntry_, _statIinit) <> _statIflow <> fromList [(label, labelExit_) | label <- toList _statIfinal]
                   {-# LINE 1241 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 268 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1246 "AttributeGrammar.hs" #-}
                   )
              _statOcontinueLabel =
                  ({-# LINE 269 "AttributeGrammar.ag" #-}
                   Nothing
                   {-# LINE 1251 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _statIstrongLive
                   {-# LINE 1256 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _statIvalSpace
                   {-# LINE 1261 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Proc' labelEntry_ labelExit_ name_ inp_ out_ _statIself
              _lhsOself =
                  _self
              _statOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIvalSpace
                   {-# LINE 1270 "AttributeGrammar.hs" #-}
                   )
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar _statOvalSpace
          in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
                   {-# LINE 1315 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 130 "AttributeGrammar.ag" #-}
                   _hdIjump : _tlIdStar
                   {-# LINE 1320 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _tlIlabel
                   {-# LINE 1329 "AttributeGrammar.hs" #-}
                   )
              _hdOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _lhsIlabel
                   {-# LINE 1334 "AttributeGrammar.hs" #-}
                   )
              _tlOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _hdIlabel
                   {-# LINE 1339 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1356 "AttributeGrammar.hs" #-}
                   )
              _lhsOdStar =
                  ({-# LINE 127 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1361 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
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
type T_Procs' = ( [(String, (Int, Int))] ) ->
                ( M.Map Int (M.Map String ConstLat) ) ->
                ( ( Set (Int, Int) ),( [String] ),Procs',( M.Map Int (Set String -> Set String) ),( M.Map Int (M.Map String ConstLat) ))
data Inh_Procs' = Inh_Procs' {dStar_Inh_Procs' :: ( [(String, (Int, Int))] ),valSpace_Inh_Procs' :: ( M.Map Int (M.Map String ConstLat) )}
data Syn_Procs' = Syn_Procs' {flow_Syn_Procs' :: ( Set (Int, Int) ),pretty_Syn_Procs' :: ( [String] ),self_Syn_Procs' :: Procs',strongLive_Syn_Procs' :: ( M.Map Int (Set String -> Set String) ),valSpace_Syn_Procs' :: ( M.Map Int (M.Map String ConstLat) )}
wrap_Procs' :: (T_Procs') ->
               (Inh_Procs') ->
               (Syn_Procs')
wrap_Procs' sem (Inh_Procs' _lhsIdStar _lhsIvalSpace) =
    (let ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIdStar _lhsIvalSpace
     in  (Syn_Procs' _lhsOflow _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Procs'_Cons :: (T_Proc') ->
                   (T_Procs') ->
                   (T_Procs')
sem_Procs'_Cons hd_ tl_ =
    (\ _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Procs'
              _hdOdStar :: ( [(String, (Int, Int))] )
              _hdOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _tlOdStar :: ( [(String, (Int, Int))] )
              _tlOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _hdIfinal :: ( Set Int )
              _hdIflow :: ( Set (Int, Int) )
              _hdIinit :: Int
              _hdIpretty :: ( [String] )
              _hdIself :: Proc'
              _hdIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _hdIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _tlIflow :: ( Set (Int, Int) )
              _tlIpretty :: ( [String] )
              _tlIself :: Procs'
              _tlIstrongLive :: ( M.Map Int (Set String -> Set String) )
              _tlIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOpretty =
                  ({-# LINE 257 "AttributeGrammar.ag" #-}
                   _hdIpretty ++ _tlIpretty
                   {-# LINE 1422 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 258 "AttributeGrammar.ag" #-}
                   _hdIflow <> _tlIflow
                   {-# LINE 1427 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _hdIstrongLive <> _tlIstrongLive
                   {-# LINE 1432 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _hdIvalSpace <> _tlIvalSpace
                   {-# LINE 1437 "AttributeGrammar.hs" #-}
                   )
              _self =
                  (:) _hdIself _tlIself
              _lhsOself =
                  _self
              _hdOdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1446 "AttributeGrammar.hs" #-}
                   )
              _hdOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIvalSpace
                   {-# LINE 1451 "AttributeGrammar.hs" #-}
                   )
              _tlOdStar =
                  ({-# LINE 199 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 1456 "AttributeGrammar.hs" #-}
                   )
              _tlOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _hdIvalSpace
                   {-# LINE 1461 "AttributeGrammar.hs" #-}
                   )
              ( _hdIfinal,_hdIflow,_hdIinit,_hdIpretty,_hdIself,_hdIstrongLive,_hdIvalSpace) =
                  hd_ _hdOdStar _hdOvalSpace
              ( _tlIflow,_tlIpretty,_tlIself,_tlIstrongLive,_tlIvalSpace) =
                  tl_ _tlOdStar _tlOvalSpace
          in  ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Procs'_Nil :: (T_Procs')
sem_Procs'_Nil =
    (\ _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Procs'
              _lhsOpretty =
                  ({-# LINE 254 "AttributeGrammar.ag" #-}
                   []
                   {-# LINE 1480 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 255 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 1485 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1490 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 1495 "AttributeGrammar.hs" #-}
                   )
              _self =
                  []
              _lhsOself =
                  _self
          in  ( _lhsOflow,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
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
              {-# LINE 1538 "AttributeGrammar.hs" #-}
              )
         _statOlabel =
             ({-# LINE 121 "AttributeGrammar.ag" #-}
              _procsIlabel
              {-# LINE 1543 "AttributeGrammar.hs" #-}
              )
         _lhsOlabelled =
             ({-# LINE 122 "AttributeGrammar.ag" #-}
              Program' _procsIlabelled _statIlabelled _procsIdStar
              {-# LINE 1548 "AttributeGrammar.hs" #-}
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
type T_Program' = ( ( Set Int ),( Set (Int, Int) ),Int,String,Program',( M.Map Int (Set String -> Set String) ),( M.Map Int (M.Map String ConstLat) ))
data Inh_Program' = Inh_Program' {}
data Syn_Program' = Syn_Program' {final_Syn_Program' :: ( Set Int ),flow_Syn_Program' :: ( Set (Int, Int) ),init_Syn_Program' :: Int,pretty_Syn_Program' :: String,self_Syn_Program' :: Program',strongLive_Syn_Program' :: ( M.Map Int (Set String -> Set String) ),valSpace_Syn_Program' :: ( M.Map Int (M.Map String ConstLat) )}
wrap_Program' :: (T_Program') ->
                 (Inh_Program') ->
                 (Syn_Program')
wrap_Program' sem (Inh_Program') =
    (let ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem
     in  (Syn_Program' _lhsOfinal _lhsOflow _lhsOinit _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Program'_Program' :: (T_Procs') ->
                         (T_Stat') ->
                         ( [(String, (Int, Int))] ) ->
                         (T_Program')
sem_Program'_Program' procs_ stat_ dStar_ =
    (let _lhsOpretty :: String
         _lhsOinit :: Int
         _lhsOfinal :: ( Set Int )
         _lhsOflow :: ( Set (Int, Int) )
         _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
         _statOdStar :: ( [(String, (Int, Int))] )
         _statOcontinueLabel :: ( Maybe Int )
         _procsOdStar :: ( [(String, (Int, Int))] )
         _procsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
         _lhsOself :: Program'
         _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
         _statOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
         _procsIflow :: ( Set (Int, Int) )
         _procsIpretty :: ( [String] )
         _procsIself :: Procs'
         _procsIstrongLive :: ( M.Map Int (Set String -> Set String) )
         _procsIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
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
         _statIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
         _lhsOpretty =
             ({-# LINE 241 "AttributeGrammar.ag" #-}
              unlines ("begin" : indent _procsIpretty ++ indent _statIpretty ++ ["end"])
              {-# LINE 1613 "AttributeGrammar.hs" #-}
              )
         _lhsOinit =
             ({-# LINE 242 "AttributeGrammar.ag" #-}
              _statIinit
              {-# LINE 1618 "AttributeGrammar.hs" #-}
              )
         _lhsOfinal =
             ({-# LINE 243 "AttributeGrammar.ag" #-}
              _statIfinal
              {-# LINE 1623 "AttributeGrammar.hs" #-}
              )
         _lhsOflow =
             ({-# LINE 244 "AttributeGrammar.ag" #-}
              _procsIflow <> _statIflow
              {-# LINE 1628 "AttributeGrammar.hs" #-}
              )
         _lhsOstrongLive =
             ({-# LINE 245 "AttributeGrammar.ag" #-}
              _procsIstrongLive <> _statIstrongLive
              {-# LINE 1633 "AttributeGrammar.hs" #-}
              )
         _statOdStar =
             ({-# LINE 246 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1638 "AttributeGrammar.hs" #-}
              )
         _statOcontinueLabel =
             ({-# LINE 247 "AttributeGrammar.ag" #-}
              Nothing
              {-# LINE 1643 "AttributeGrammar.hs" #-}
              )
         _procsOdStar =
             ({-# LINE 248 "AttributeGrammar.ag" #-}
              dStar_
              {-# LINE 1648 "AttributeGrammar.hs" #-}
              )
         _procsOvalSpace =
             ({-# LINE 250 "AttributeGrammar.ag" #-}
              M.empty
              {-# LINE 1653 "AttributeGrammar.hs" #-}
              )
         _self =
             Program' _procsIself _statIself dStar_
         _lhsOself =
             _self
         _lhsOvalSpace =
             ({-# LINE 197 "AttributeGrammar.ag" #-}
              _statIvalSpace
              {-# LINE 1662 "AttributeGrammar.hs" #-}
              )
         _statOvalSpace =
             ({-# LINE 204 "AttributeGrammar.ag" #-}
              _procsIvalSpace
              {-# LINE 1667 "AttributeGrammar.hs" #-}
              )
         ( _procsIflow,_procsIpretty,_procsIself,_procsIstrongLive,_procsIvalSpace) =
             procs_ _procsOdStar _procsOvalSpace
         ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
             stat_ _statOcontinueLabel _statOdStar _statOvalSpace
     in  ( _lhsOfinal,_lhsOflow,_lhsOinit,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace))
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
                   {-# LINE 1735 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 142 "AttributeGrammar.ag" #-}
                   Skip' _lhsIlabel
                   {-# LINE 1740 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1767 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 146 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1772 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 147 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1777 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 148 "AttributeGrammar.ag" #-}
                   IfThenElse' _lhsIlabel cond_ _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1782 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1808 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabel =
                  ({-# LINE 152 "AttributeGrammar.ag" #-}
                   _statIlabel
                   {-# LINE 1813 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 153 "AttributeGrammar.ag" #-}
                   While' _lhsIlabel cond_ _statIlabelled
                   {-# LINE 1818 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1839 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 157 "AttributeGrammar.ag" #-}
                   Call' _lhsIlabel (_lhsIlabel + 1) name_ params_ out_
                   {-# LINE 1844 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1862 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 161 "AttributeGrammar.ag" #-}
                   IAssign' _lhsIlabel name_ val_
                   {-# LINE 1867 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1885 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 165 "AttributeGrammar.ag" #-}
                   BAssign' _lhsIlabel name_ val_
                   {-# LINE 1890 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1916 "AttributeGrammar.hs" #-}
                   )
              _stat2Olabel =
                  ({-# LINE 169 "AttributeGrammar.ag" #-}
                   _stat1Ilabel
                   {-# LINE 1921 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 170 "AttributeGrammar.ag" #-}
                   Seq' _stat1Ilabelled _stat2Ilabelled
                   {-# LINE 1926 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOlabel =
                  ({-# LINE 111 "AttributeGrammar.ag" #-}
                   _stat2Ilabel
                   {-# LINE 1935 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1953 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 174 "AttributeGrammar.ag" #-}
                   Malloc' _lhsIlabel name_ size_
                   {-# LINE 1958 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1975 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 178 "AttributeGrammar.ag" #-}
                   Free' _lhsIlabel ptr_
                   {-# LINE 1980 "AttributeGrammar.hs" #-}
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
                   {-# LINE 1998 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 182 "AttributeGrammar.ag" #-}
                   RefAssign' _lhsIlabel ptr_ val_
                   {-# LINE 2003 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2019 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 186 "AttributeGrammar.ag" #-}
                   Continue' _lhsIlabel
                   {-# LINE 2024 "AttributeGrammar.hs" #-}
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
                   {-# LINE 2040 "AttributeGrammar.hs" #-}
                   )
              _lhsOlabelled =
                  ({-# LINE 190 "AttributeGrammar.ag" #-}
                   Break' _lhsIlabel
                   {-# LINE 2045 "AttributeGrammar.hs" #-}
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
               ( M.Map Int (M.Map String ConstLat) ) ->
               ( ( Set Int ),( Maybe Int ),( Set Int ),( Set (Int, Int) ),Int,Bool,Bool,( [String] ),Stat',( M.Map Int (Set String -> Set String) ),( M.Map Int (M.Map String ConstLat) ))
data Inh_Stat' = Inh_Stat' {continueLabel_Inh_Stat' :: ( Maybe Int ),dStar_Inh_Stat' :: ( [(String, (Int, Int))] ),valSpace_Inh_Stat' :: ( M.Map Int (M.Map String ConstLat) )}
data Syn_Stat' = Syn_Stat' {breakLabels_Syn_Stat' :: ( Set Int ),continueLabel_Syn_Stat' :: ( Maybe Int ),final_Syn_Stat' :: ( Set Int ),flow_Syn_Stat' :: ( Set (Int, Int) ),init_Syn_Stat' :: Int,isSingle_Syn_Stat' :: Bool,isSkip_Syn_Stat' :: Bool,pretty_Syn_Stat' :: ( [String] ),self_Syn_Stat' :: Stat',strongLive_Syn_Stat' :: ( M.Map Int (Set String -> Set String) ),valSpace_Syn_Stat' :: ( M.Map Int (M.Map String ConstLat) )}
wrap_Stat' :: (T_Stat') ->
              (Inh_Stat') ->
              (Syn_Stat')
wrap_Stat' sem (Inh_Stat' _lhsIcontinueLabel _lhsIdStar _lhsIvalSpace) =
    (let ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace) = sem _lhsIcontinueLabel _lhsIdStar _lhsIvalSpace
     in  (Syn_Stat' _lhsObreakLabels _lhsOcontinueLabel _lhsOfinal _lhsOflow _lhsOinit _lhsOisSingle _lhsOisSkip _lhsOpretty _lhsOself _lhsOstrongLive _lhsOvalSpace))
sem_Stat'_Skip' :: Int ->
                   (T_Stat')
sem_Stat'_Skip' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 273 "AttributeGrammar.ag" #-}
                   ["skip" ++ showLabel label_]
                   {-# LINE 2126 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 274 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2131 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 275 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2136 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 276 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2141 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 277 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2146 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 278 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2151 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2156 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2161 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2166 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Skip' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2175 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IfThenElse' :: Int ->
                         T_BExpr ->
                         (T_Stat') ->
                         (T_Stat') ->
                         (T_Stat')
sem_Stat'_IfThenElse' labelc_ cond_ stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat1OvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _stat2OvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _condIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
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
              _stat1IvalSpace :: ( M.Map Int (M.Map String ConstLat) )
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
              _stat2IvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOpretty =
                  ({-# LINE 281 "AttributeGrammar.ag" #-}
                   ["if [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " then" ++ (if _stat1IisSingle then "" else " {")]
                    ++ indent _stat1Ipretty
                    ++ (if _stat2IisSkip then (if _stat1IisSingle then [] else ["}"]) else
                            [(if _stat1IisSingle then "" else "} ") ++ "else" ++ (if _stat2IisSingle then "" else " {")]
                        ++ indent _stat2Ipretty
                        ++ (if _stat2IisSingle then [] else ["}"])
                      )
                   {-# LINE 2240 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 288 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2245 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 289 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2250 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 290 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2255 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 291 "AttributeGrammar.ag" #-}
                   _stat1Ifinal <> _stat2Ifinal
                   {-# LINE 2260 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 292 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(labelc_, _stat1Iinit), (labelc_, _stat2Iinit)]
                   {-# LINE 2265 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 293 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive <> M.fromList [(labelc_, (<> _condIfreeVars))]
                   {-# LINE 2270 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2275 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2280 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IfThenElse' labelc_ _condIself _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2289 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2294 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2299 "AttributeGrammar.hs" #-}
                   )
              _stat1OvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIvalSpace
                   {-# LINE 2304 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2309 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2314 "AttributeGrammar.hs" #-}
                   )
              _stat2OvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1IvalSpace
                   {-# LINE 2319 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1OvalSpace
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2OvalSpace
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_While' :: Int ->
                    T_BExpr ->
                    (T_Stat') ->
                    (T_Stat')
sem_Stat'_While' labelc_ cond_ stat_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _statOcontinueLabel :: ( Maybe Int )
              _statOdStar :: ( [(String, (Int, Int))] )
              _statOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _condIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
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
              _statIvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOpretty =
                  ({-# LINE 296 "AttributeGrammar.ag" #-}
                   ["while [" ++ _condIpretty ++ "]" ++ showLabel labelc_ ++ " do" ++ (if _statIisSingle then "" else " {")]
                    ++ indent _statIpretty
                    ++ (if _statIisSingle then [] else ["}"])
                   {-# LINE 2371 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 299 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2376 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 300 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2381 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 301 "AttributeGrammar.ag" #-}
                   labelc_
                   {-# LINE 2386 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 302 "AttributeGrammar.ag" #-}
                   singleton labelc_ <> _statIbreakLabels
                   {-# LINE 2391 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 303 "AttributeGrammar.ag" #-}
                   _statIflow <> singleton (labelc_, _statIinit) <> fromList [(label, labelc_) | label <- toList _statIfinal ]
                   {-# LINE 2396 "AttributeGrammar.hs" #-}
                   )
              _lhsOcontinueLabel =
                  ({-# LINE 304 "AttributeGrammar.ag" #-}
                   Just labelc_
                   {-# LINE 2401 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 305 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2406 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 306 "AttributeGrammar.ag" #-}
                   _statIstrongLive <> M.fromList [(labelc_, (<> _condIfreeVars))]
                   {-# LINE 2411 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _statIvalSpace
                   {-# LINE 2416 "AttributeGrammar.hs" #-}
                   )
              _self =
                  While' labelc_ _condIself _statIself
              _lhsOself =
                  _self
              _statOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2425 "AttributeGrammar.hs" #-}
                   )
              _statOdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2430 "AttributeGrammar.hs" #-}
                   )
              _statOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIvalSpace
                   {-# LINE 2435 "AttributeGrammar.hs" #-}
                   )
              ( _condIexpValSpace,_condIfreeVars,_condIprecedence,_condIpretty,_condIself) =
                  cond_
              ( _statIbreakLabels,_statIcontinueLabel,_statIfinal,_statIflow,_statIinit,_statIisSingle,_statIisSkip,_statIpretty,_statIself,_statIstrongLive,_statIvalSpace) =
                  stat_ _statOcontinueLabel _statOdStar _statOvalSpace
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Call' :: Int ->
                   Int ->
                   String ->
                   T_Exprs ->
                   String ->
                   (T_Stat')
sem_Stat'_Call' labelCall_ labelReturn_ name_ params_ out_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _paramsIfreeVars :: ( Set String )
              _paramsIpretty :: String
              _paramsIself :: Exprs
              _lhsOpretty =
                  ({-# LINE 309 "AttributeGrammar.ag" #-}
                   ["[call " ++ name_ ++ "(" ++ _paramsIpretty ++ out_ ++ ")]" ++ showLabel labelCall_ ++ "₋" ++ showLabel labelReturn_]
                   {-# LINE 2469 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 310 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2474 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 311 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2479 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 312 "AttributeGrammar.ag" #-}
                   labelCall_
                   {-# LINE 2484 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 313 "AttributeGrammar.ag" #-}
                   singleton labelReturn_
                   {-# LINE 2489 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 314 "AttributeGrammar.ag" #-}
                   let (l_n, l_x) = Maybe.fromJust $ lookup name_ _lhsIdStar in
                   fromList [(labelCall_, l_n), (l_x, labelReturn_)]
                   {-# LINE 2495 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2500 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2505 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2510 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Call' labelCall_ labelReturn_ name_ _paramsIself out_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2519 "AttributeGrammar.hs" #-}
                   )
              ( _paramsIfreeVars,_paramsIpretty,_paramsIself) =
                  params_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_IAssign' :: Int ->
                      String ->
                      T_IExpr ->
                      (T_Stat')
sem_Stat'_IAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 322 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2551 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 323 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2556 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 324 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2561 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 325 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2566 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 326 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2571 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 327 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2576 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 328 "AttributeGrammar.ag" #-}
                   M.fromList [(label_, survive name_ _valIfreeVars)]
                   {-# LINE 2581 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 329 "AttributeGrammar.ag" #-}
                   M.fromList [(label_, testConstant name_ (_valIexpValSpace (_lhsIvalSpace M.! label_)) (_lhsIvalSpace M.! label_))]
                   {-# LINE 2586 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2591 "AttributeGrammar.hs" #-}
                   )
              _self =
                  IAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2600 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_BAssign' :: Int ->
                      String ->
                      T_BExpr ->
                      (T_Stat')
sem_Stat'_BAssign' label_ name_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _valIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: BExpr
              _lhsOpretty =
                  ({-# LINE 332 "AttributeGrammar.ag" #-}
                   ["[" ++ name_ ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 2632 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 333 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2637 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 334 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2642 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 335 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2647 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 336 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2652 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 337 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2657 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 338 "AttributeGrammar.ag" #-}
                   M.fromList [(label_, survive name_ _valIfreeVars)]
                   {-# LINE 2662 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 339 "AttributeGrammar.ag" #-}
                   M.fromList [(label_, testConstant name_ (_valIexpValSpace (_lhsIvalSpace M.! label_)) (_lhsIvalSpace M.! label_))]
                   {-# LINE 2667 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2672 "AttributeGrammar.hs" #-}
                   )
              _self =
                  BAssign' label_ name_ _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2681 "AttributeGrammar.hs" #-}
                   )
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Seq' :: (T_Stat') ->
                  (T_Stat') ->
                  (T_Stat')
sem_Stat'_Seq' stat1_ stat2_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _stat1OcontinueLabel :: ( Maybe Int )
              _stat1OdStar :: ( [(String, (Int, Int))] )
              _stat1OvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _stat2OcontinueLabel :: ( Maybe Int )
              _stat2OdStar :: ( [(String, (Int, Int))] )
              _stat2OvalSpace :: ( M.Map Int (M.Map String ConstLat) )
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
              _stat1IvalSpace :: ( M.Map Int (M.Map String ConstLat) )
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
              _stat2IvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOpretty =
                  ({-# LINE 342 "AttributeGrammar.ag" #-}
                   addSemicolon _stat1Ipretty ++ _stat2Ipretty
                   {-# LINE 2735 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 343 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2740 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 344 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2745 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 345 "AttributeGrammar.ag" #-}
                   _stat1Iinit
                   {-# LINE 2750 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 346 "AttributeGrammar.ag" #-}
                   _stat2Ifinal
                   {-# LINE 2755 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 347 "AttributeGrammar.ag" #-}
                   _stat1Iflow <> _stat2Iflow <> fromList [(label, _stat2Iinit) | label <- toList _stat1Ifinal]
                   {-# LINE 2760 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat1IbreakLabels <> _stat2IbreakLabels
                   {-# LINE 2765 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1IstrongLive <> _stat2IstrongLive
                   {-# LINE 2770 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1IvalSpace <> _stat2IvalSpace
                   {-# LINE 2775 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Seq' _stat1Iself _stat2Iself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat2IcontinueLabel
                   {-# LINE 2784 "AttributeGrammar.hs" #-}
                   )
              _stat1OcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2789 "AttributeGrammar.hs" #-}
                   )
              _stat1OdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2794 "AttributeGrammar.hs" #-}
                   )
              _stat1OvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _lhsIvalSpace
                   {-# LINE 2799 "AttributeGrammar.hs" #-}
                   )
              _stat2OcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _stat1IcontinueLabel
                   {-# LINE 2804 "AttributeGrammar.hs" #-}
                   )
              _stat2OdStar =
                  ({-# LINE 198 "AttributeGrammar.ag" #-}
                   _lhsIdStar
                   {-# LINE 2809 "AttributeGrammar.hs" #-}
                   )
              _stat2OvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   _stat1IvalSpace
                   {-# LINE 2814 "AttributeGrammar.hs" #-}
                   )
              ( _stat1IbreakLabels,_stat1IcontinueLabel,_stat1Ifinal,_stat1Iflow,_stat1Iinit,_stat1IisSingle,_stat1IisSkip,_stat1Ipretty,_stat1Iself,_stat1IstrongLive,_stat1IvalSpace) =
                  stat1_ _stat1OcontinueLabel _stat1OdStar _stat1OvalSpace
              ( _stat2IbreakLabels,_stat2IcontinueLabel,_stat2Ifinal,_stat2Iflow,_stat2Iinit,_stat2IisSingle,_stat2IisSkip,_stat2Ipretty,_stat2Iself,_stat2IstrongLive,_stat2IvalSpace) =
                  stat2_ _stat2OcontinueLabel _stat2OdStar _stat2OvalSpace
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Malloc' :: Int ->
                     String ->
                     T_IExpr ->
                     (T_Stat')
sem_Stat'_Malloc' label_ name_ size_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _sizeIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _sizeIfreeVars :: ( Set String )
              _sizeIprecedence :: Int
              _sizeIpretty :: String
              _sizeIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 350 "AttributeGrammar.ag" #-}
                   ["malloc(" ++ name_ ++ ", " ++ _sizeIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2848 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 351 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2853 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 352 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2858 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 353 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2863 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 354 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2868 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 355 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2873 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 356 "AttributeGrammar.ag" #-}
                   M.fromList [(label_, survive name_ _sizeIfreeVars)]
                   {-# LINE 2878 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2883 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2888 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Malloc' label_ name_ _sizeIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2897 "AttributeGrammar.hs" #-}
                   )
              ( _sizeIexpValSpace,_sizeIfreeVars,_sizeIprecedence,_sizeIpretty,_sizeIself) =
                  size_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Free' :: Int ->
                   T_IExpr ->
                   (T_Stat')
sem_Stat'_Free' label_ ptr_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 360 "AttributeGrammar.ag" #-}
                   ["free(" ++ _ptrIpretty ++ ")" ++ showLabel label_]
                   {-# LINE 2928 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 361 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 2933 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 362 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 2938 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 363 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 2943 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 364 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 2948 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 365 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2953 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 2958 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2963 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 2968 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Free' label_ _ptrIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 2977 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_RefAssign' :: Int ->
                        T_IExpr ->
                        T_IExpr ->
                        (T_Stat')
sem_Stat'_RefAssign' label_ ptr_ val_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _ptrIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _ptrIfreeVars :: ( Set String )
              _ptrIprecedence :: Int
              _ptrIpretty :: String
              _ptrIself :: IExpr
              _valIexpValSpace :: ( M.Map String ConstLat -> ConstLat )
              _valIfreeVars :: ( Set String )
              _valIprecedence :: Int
              _valIpretty :: String
              _valIself :: IExpr
              _lhsOpretty =
                  ({-# LINE 369 "AttributeGrammar.ag" #-}
                   ["[*" ++ _ptrIpretty ++ " := " ++ _valIpretty ++ "]" ++ showLabel label_]
                   {-# LINE 3014 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 370 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3019 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 371 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3024 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 372 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3029 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 373 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3034 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 374 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3039 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3044 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3049 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3054 "AttributeGrammar.hs" #-}
                   )
              _self =
                  RefAssign' label_ _ptrIself _valIself
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3063 "AttributeGrammar.hs" #-}
                   )
              ( _ptrIexpValSpace,_ptrIfreeVars,_ptrIprecedence,_ptrIpretty,_ptrIself) =
                  ptr_
              ( _valIexpValSpace,_valIfreeVars,_valIprecedence,_valIpretty,_valIself) =
                  val_
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Continue' :: Int ->
                       (T_Stat')
sem_Stat'_Continue' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 378 "AttributeGrammar.ag" #-}
                   ["continue" ++ showLabel label_]
                   {-# LINE 3090 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 379 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3095 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 380 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3100 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 381 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3105 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 382 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3110 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 383 "AttributeGrammar.ag" #-}
                   singleton (label_, Maybe.fromJust _lhsIcontinueLabel)
                   {-# LINE 3115 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3120 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3125 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3130 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Continue' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3139 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))
sem_Stat'_Break' :: Int ->
                    (T_Stat')
sem_Stat'_Break' label_ =
    (\ _lhsIcontinueLabel
       _lhsIdStar
       _lhsIvalSpace ->
         (let _lhsOpretty :: ( [String] )
              _lhsOisSkip :: Bool
              _lhsOisSingle :: Bool
              _lhsOinit :: Int
              _lhsOfinal :: ( Set Int )
              _lhsOflow :: ( Set (Int, Int) )
              _lhsObreakLabels :: ( Set Int )
              _lhsOstrongLive :: ( M.Map Int (Set String -> Set String) )
              _lhsOvalSpace :: ( M.Map Int (M.Map String ConstLat) )
              _lhsOself :: Stat'
              _lhsOcontinueLabel :: ( Maybe Int )
              _lhsOpretty =
                  ({-# LINE 386 "AttributeGrammar.ag" #-}
                   ["break" ++ showLabel label_]
                   {-# LINE 3162 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSkip =
                  ({-# LINE 387 "AttributeGrammar.ag" #-}
                   False
                   {-# LINE 3167 "AttributeGrammar.hs" #-}
                   )
              _lhsOisSingle =
                  ({-# LINE 388 "AttributeGrammar.ag" #-}
                   True
                   {-# LINE 3172 "AttributeGrammar.hs" #-}
                   )
              _lhsOinit =
                  ({-# LINE 389 "AttributeGrammar.ag" #-}
                   label_
                   {-# LINE 3177 "AttributeGrammar.hs" #-}
                   )
              _lhsOfinal =
                  ({-# LINE 390 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3182 "AttributeGrammar.hs" #-}
                   )
              _lhsOflow =
                  ({-# LINE 391 "AttributeGrammar.ag" #-}
                   empty
                   {-# LINE 3187 "AttributeGrammar.hs" #-}
                   )
              _lhsObreakLabels =
                  ({-# LINE 392 "AttributeGrammar.ag" #-}
                   singleton label_
                   {-# LINE 3192 "AttributeGrammar.hs" #-}
                   )
              _lhsOstrongLive =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3197 "AttributeGrammar.hs" #-}
                   )
              _lhsOvalSpace =
                  ({-# LINE 204 "AttributeGrammar.ag" #-}
                   M.empty
                   {-# LINE 3202 "AttributeGrammar.hs" #-}
                   )
              _self =
                  Break' label_
              _lhsOself =
                  _self
              _lhsOcontinueLabel =
                  ({-# LINE 203 "AttributeGrammar.ag" #-}
                   _lhsIcontinueLabel
                   {-# LINE 3211 "AttributeGrammar.hs" #-}
                   )
          in  ( _lhsObreakLabels,_lhsOcontinueLabel,_lhsOfinal,_lhsOflow,_lhsOinit,_lhsOisSingle,_lhsOisSkip,_lhsOpretty,_lhsOself,_lhsOstrongLive,_lhsOvalSpace)))