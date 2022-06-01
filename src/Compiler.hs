module Compiler where

import Analyses (DifTrans, Dir (..), Edge, Inter (..), lookupR)
import AttributeGrammar
import ConstantBranch (ConstBranchLat)
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
import Data.Map qualified as M
import Data.Set qualified as S
import Std (Map, Set, intercalate, intersperse)
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

  let
    constantPropA =
      Analysis Forward (valSpace_Syn_Program' synProgram') (Just constEmpty)
  let
    constantPropagationMonotoneFramework :: MonotoneFramework PtConstLat
    constantPropagationMonotoneFramework = prepare constantPropA flow
  let
    constantPropagationEmbellishedMonotoneFramework ::
      MonotoneFramework (ContextSensitive PtConstLat)
    constantPropagationEmbellishedMonotoneFramework =
      contextSensitize callStringLimit constantPropagationMonotoneFramework

  let
    constantBranchA =
      Analysis Forward (constBranchT_Syn_Program' synProgram') (Just constEmpty, mempty)
  let
    constantPropagationBranchAwareMonotoneFramework :: MonotoneFramework ConstBranchLat
    constantPropagationBranchAwareMonotoneFramework = prepare constantBranchA flow
  let
    constantPropagationBranchAwareEmbellishedMonotoneFramework ::
      MonotoneFramework (ContextSensitive ConstBranchLat)
    constantPropagationBranchAwareEmbellishedMonotoneFramework =
      contextSensitize callStringLimit constantPropagationBranchAwareMonotoneFramework

  let
    strongLiveA =
      Analysis Backward (strongLive_Syn_Program' synProgram') mempty
  let
    stronglyLiveVariablesMonotoneFramework :: MonotoneFramework (Set String)
    stronglyLiveVariablesMonotoneFramework = prepare strongLiveA flow
  let
    stronglyLiveVariablesEmbellishedMonotoneFramework ::
      MonotoneFramework (ContextSensitive (Set String))
    stronglyLiveVariablesEmbellishedMonotoneFramework =
      contextSensitize callStringLimit stronglyLiveVariablesMonotoneFramework

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
  prettyPrint constantPropA $
    mfpSolution constantPropagationEmbellishedMonotoneFramework

  putStrLn "## Reachable Constant Propagation"
  prettyPrint constantBranchA $
    mfpSolution constantPropagationBranchAwareEmbellishedMonotoneFramework

  putStrLn ""
  putStrLn "## Strongly Live Variables"
  prettyPrint strongLiveA $
    mfpSolution stronglyLiveVariablesEmbellishedMonotoneFramework


prettyPrint ::
  (Show propertySpace) =>
  Analysis propertySpace ->
  (
    Map Label (ContextSensitive propertySpace),
    Map Label (ContextSensitive propertySpace),
    [(Map Label (ContextSensitive propertySpace), [(Label, Label)])]
  ) ->
  IO ()
prettyPrint (Analysis {direction}) (solution_circ, solution_bullet, _steps) =
  putStrLn "### entry properties" *>
  p entry *>
  putStrLn "### exit properties" *>
  p exit
  where
    entry =
      case direction of
        Forward -> solution_circ
        Backward -> solution_bullet
    exit =
      case direction of
        Forward -> solution_bullet
        Backward -> solution_circ
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

latexPrint ::
  (Maybe propertySpace -> String) ->
  (
    Map Label (ContextSensitive propertySpace),
    Map Label (ContextSensitive propertySpace),
    [(Map Label (ContextSensitive propertySpace), [(Label, Label)])]
  ) ->
  IO ()
latexPrint p (entry, exit, _steps) =
  putStr "\\[" *>
  putStr (latexPrinter "Entry" p entry) *>
  putStrLn "\\hspace{1em}" *>
  putStr (latexPrinter "Exit" p exit) *>
  putStrLn "\\]" *>
  putStrLn ""

latexPrinter :: String -> (Maybe propertySpace -> String) -> Map Label (ContextSensitive propertySpace) -> String
latexPrinter name p a = unlines (header : "\\hline" : labels :  body ++ [footer])
  where
  analysis  = fmap runTotalMap a
  analysisT = flipMap analysis
  rows      = M.keys analysis
  cols      = M.keys analysisT

  findAll ks m = fmap (`M.lookup` m) ks
  printRow i   = show i ++ " & " ++ intercalate " & " (fmap p (findAll cols $ analysis M.! i)) ++ " \\\\"

  header = "\\begin{array}{|" ++ intercalate "|" (replicate (1 + length cols) "c") ++ "|}"
  labels = "\\text{" ++ name ++ "} & {" ++ intercalate "} & {" (fmap show cols) ++ "}\\\\"
  body   = ["\\hline"] ++ intersperse "\\hline" (fmap printRow rows) ++ ["\\hline"]
  footer = "\\end{array}"

constantPropagationTex :: Maybe PtConstLat -> String
constantPropagationTex (Just (Just e)) = constEnvTex e
constantPropagationTex _                          = ""

constEnvTex :: ConstEnv -> String
constEnvTex (ConstEnv e) = intercalate ", " $ p <$> M.toList e
  where
    p (k, CI n) = k ++ " \\mapsto " ++ show n
    p (k, CB n) = k ++ " \\mapsto " ++ show n

constantBranchTex :: Maybe (PtConstLat, Set Int) -> String
constantBranchTex Nothing       = ""
constantBranchTex (Just (c, d)) = pc c ++ "; " ++ setTex show d
  where
    pc Nothing  = "\\bot"
    pc (Just e) = constEnvTex e

setTex :: (a -> String) -> Set a -> String
setTex p s
  | S.null s = "\\emptyset"
  | otherwise   = "\\{" ++ intercalate ", " (p <$> S.toList s) ++ "\\}"

strongLiveTex :: Maybe (Set String) -> String
strongLiveTex Nothing  = "\\bot"
strongLiveTex (Just x) = setTex id x

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
