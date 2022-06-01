module AnalysesConversion where

import Analyses (DifTrans, Edge, Inter (..), lookupR)
import AttributeGrammar
import ConstantBranch (ConstBranchLat, Intersect(..))
import ConstantProp (ConstEnv (ConstEnv), ConstLat (..), PtConstLat, constEmpty)
import ContextSensitive
  ( ContextSensitive (runTotalMap),
    contextSensitize,
  )
import Lexer (alex)
import MonotoneFrameworks
  ( InterproceduralFragment (..),
    Label,
    MonotoneFramework (MonotoneFramework),
    mfpSolution,
  )
import Parser (happy)

import Control.Arrow (Arrow ((&&&)))
import qualified Data.Map as M
import qualified Data.Set as S
import Std (Map, Set, intercalate, intersperse)
    
data Dir = Forward | Backward deriving (Show, Eq)
data Flow = Flow {
  initial   :: Int,
  finals    :: Set Int,
  edges     :: Set Edge,
  interflow :: Set Inter
}

data Analysis p = Analysis {
  direction :: Dir,
  difTrans   :: DifTrans p,
  extremal   :: p
}

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

unpackFlow :: Bool -> Flow -> (Set Int, Set Edge, Set Inter)
unpackFlow backward flow = (a, b, c)
  where
    a = if backward then finals flow else S.singleton (initial flow)

    es = edges flow
    b = if backward then S.map swap es else es

    swapInter (Inter x y z w) = Inter w z y x
    is = interflow flow
    c = if backward then S.map swapInter is else is

group :: (Ord a, Ord b) => [(a, b)] -> M.Map a (Set b)
group []          = mempty
group ((a, b):xs) = M.alter h a $ group xs
  where
    h Nothing  = Just $ S.singleton b
    h (Just u) = Just $ S.insert b u

prepare :: Analysis p -> Flow -> MonotoneFramework p
prepare ana flow =
  MonotoneFramework
    outgoing
    exL
    exV
    transferFunctions'
    (InterproceduralFragment $
      M.fromList [(c, (r, lookupR r trans)) | Inter c _ _ r <- S.toList inter]
    )
  where
    exV  = extremal ana
    trans = difTrans ana

    (exL, flow', inter) = unpackFlow (direction ana == Backward) flow

    outgoing = group $ S.toList flow'

    (transferFunctions', _) = trans
