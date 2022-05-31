module Compiler where

-- to-do. explicify

import Analyses (DifTrans, Dir (..), Edge, Inter (..), lookupR)
import AttributeGrammar
import ConstantBranch (ConstBranchLat)
import ConstantProp (PtConstLat, constEmpty)
import Control.Arrow (Arrow ((&&&)))
import Data.Map qualified as M
import Data.Set qualified as S
import Lexer (alex)
import MonotoneFrameworks
  ( ContextSensitive (runTotalMap),
    InterproceduralFragment (..),
    Label,
    MonotoneFramework (MonotoneFramework),
    mfpSolution',
  )
import Parser (happy)
import Std (Map, Set)
import Text.Pretty.Simple (pPrintLightBg)


callStringLimit :: Int
callStringLimit = 2

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

prepare :: Analysis p -> Flow -> (MonotoneFramework p, InterproceduralFragment p)
prepare ana flow = (a, InterproceduralFragment b)
  where
    exV  = extremal ana
    trans = difTrans ana

    (exL, flow', inter) = unpackFlow (direction ana == Backward) flow

    outgoing = group $ S.toList flow'

    (transferFunctions', _) = trans

    a = MonotoneFramework outgoing exL exV transferFunctions'
    b = M.fromList [(c, (r, lookupR r trans)) | Inter c _ _ r <- S.toList inter]



compile :: String -> IO ()
compile source = do
  let program = happy $ alex source
  let synProgram  = wrap_Program  (sem_Program  program)  Inh_Program
  let program' = labelled_Syn_Program synProgram
  let synProgram' = wrap_Program' (sem_Program' program') Inh_Program'

  putStrLn ""
  putStrLn "# Program"
  putStrLn $ pretty_Syn_Program' synProgram'


  let initial'   = init_Syn_Program' synProgram'
  let finals'    = final_Syn_Program' synProgram'
  let edges'     = flow_Syn_Program' synProgram'
  let interflow' = interflow_Syn_Program' synProgram'
  let flow = Flow initial' finals' edges' interflow'

  putStrLn ""
  putStrLn "# Output"
  putStrLn "## initial"
  print initial'
  putStrLn "## finals"
  print finals'
  putStrLn "## flow"
  print edges'
  putStrLn "## inter-flow"
  print interflow'
  putStrLn "## call string limit"
  print callStringLimit

  let constantPropA = Analysis Forward (valSpace_Syn_Program' synProgram') (Just constEmpty)
  let constantPropM = prepare constantPropA flow
  let
    constantPropagationMonotoneFramework :: MonotoneFramework PtConstLat
    constantPropagationMonotoneFramework = fst constantPropM
  let
    constantPropagationEmbellishedMonotoneFramework ::
      (MonotoneFramework PtConstLat, InterproceduralFragment PtConstLat)
    constantPropagationEmbellishedMonotoneFramework =
      (constantPropagationMonotoneFramework, snd constantPropM)

  let constantBranchA = Analysis Forward (constBranchT_Syn_Program' synProgram') (Just constEmpty, mempty)
  let constantBranchM = prepare constantBranchA flow
  let
    constantPropagationBranchAwareMonotoneFramework :: MonotoneFramework ConstBranchLat
    constantPropagationBranchAwareMonotoneFramework = fst constantBranchM
  let
    constantPropagationBranchAwareEmbellishedMonotoneFramework ::
      (MonotoneFramework ConstBranchLat, InterproceduralFragment ConstBranchLat)
    constantPropagationBranchAwareEmbellishedMonotoneFramework =
      (constantPropagationBranchAwareMonotoneFramework, snd constantBranchM)

  let strongLiveA = Analysis Backward (strongLive_Syn_Program' synProgram') mempty
  let strongLiveM = prepare strongLiveA flow
  let
    stronglyLiveVariablesMonotoneFramework :: MonotoneFramework (Set String)
    stronglyLiveVariablesMonotoneFramework = fst strongLiveM
  let
    stronglyLiveVariablesEmbellishedMonotoneFramework ::
      (MonotoneFramework (Set String), InterproceduralFragment (Set String))
    stronglyLiveVariablesEmbellishedMonotoneFramework =
      (stronglyLiveVariablesMonotoneFramework, snd strongLiveM)

  putStrLn ""
  putStrLn "# Analyses"
  putStrLn "Each analysis result will be represented as a `Map [Label] (Map Label propertySpace)`."
  putStrLn "That is, a map from call strings to a map from labels to properties."
  putStrLn "Absent combinations of call strings and labels are unreachable."
  putStrLn "Their properties are not restricted by the data flow equations and can be anything."
  putStrLn "Assume them to be bottom for the least solution."
  putStrLn "`Map`s will be printed as lists."
  putStrLn "So the printed result will look as of type `[([Label], [(Label, propertySpace)])]`."
  putStrLn "## Constant Propagation"
  prettyPrint $
    uncurry (mfpSolution' callStringLimit) constantPropagationEmbellishedMonotoneFramework

  putStrLn "## Reachable Constant Propagation"
  prettyPrint $
    uncurry (mfpSolution' callStringLimit) constantPropagationBranchAwareEmbellishedMonotoneFramework

  putStrLn ""
  putStrLn "## Strongly Live Variables"
  prettyPrint $
    uncurry (mfpSolution' callStringLimit) stronglyLiveVariablesEmbellishedMonotoneFramework


prettyPrint ::
  (Show propertySpace) =>
  (
    Map Label (ContextSensitive propertySpace),
    Map Label (ContextSensitive propertySpace),
    [(Map Label (ContextSensitive propertySpace), [(Label, Label)])]
  ) ->
  IO ()
prettyPrint (entry, exit, _steps) =
  putStrLn "### entry properties" *>
  p entry *>
  putStrLn "### exit properties" *>
  p exit
  where
    p ::
      (Show propertySpace) =>
      Map Label (ContextSensitive propertySpace) -> IO ()
    p =
      pPrintLightBg .
      M.toList .
      fmap M.toList .
      M.mapKeysWith (error "impossible. reverse is bijective.") reverse .
      flipMap .
      fmap runTotalMap

flipMap :: (Ord k, Ord m) => M.Map k (M.Map m v) -> M.Map m (M.Map k v)
flipMap = fmap M.fromList . groupBy' h . rotate . M.toList . fmap M.toList
  where
    h (k, (m, v)) = (m, (k, v))

groupBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupBy f = M.fromListWith (++) . map (f &&& pure)

groupBy' :: Ord k => (a -> (k, b)) -> [a] -> M.Map k [b]
groupBy' f = M.fromListWith (++) . map (fst . f &&& pure . (snd . f))

rotate :: [(k, [(m, v)])] -> [(k, (m, v))]
rotate = (h =<<)
  where
    h (k, xs) = fmap (k, ) xs

secondOf3 :: (a, b, c) -> b
secondOf3 (_, b, _) = b
