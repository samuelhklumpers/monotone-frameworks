module Compiler where

import qualified Data.Map as M
import qualified Data.Set as S

import Parser
import Lexer
import AttributeGrammar
import MonotoneFrameworks
import Analyses
import ConstantProp
import StrongLive

import Text.Pretty.Simple (pPrintLightBg)
import Control.Arrow

data Flow = Flow {
  initial   :: Int,
  finals    :: S.Set Int,
  edges     :: S.Set Edge,
  interflow :: S.Set Inter
}

data Analysis p = Analysis {
  direction :: Dir,
  difTrans   :: DifTrans p,
  extremal   :: p
}

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

unpackFlow :: Bool -> Flow -> (S.Set Int, S.Set Edge, S.Set Inter)
unpackFlow backward flow = (a, b, c)
  where
    a = if backward then finals flow else S.singleton (initial flow)

    es = edges flow
    b = if backward then S.map swap es else es

    swapInter (Inter x y z w) = Inter w z y x
    is = interflow flow
    c = if backward then S.map swapInter is else is

group :: (Ord a, Ord b) => [(a, b)] -> M.Map a (S.Set b)
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
  print initial'
  print finals'
  print edges'
  print interflow'


  let constantPropA = Analysis Forward (valSpace_Syn_Program' synProgram') (pure mempty)
  let constantPropM = prepare constantPropA flow

  let strongLiveA = Analysis Backward (strongLive_Syn_Program' synProgram') mempty
  let strongLiveM = prepare strongLiveA flow

  putStrLn ""
  putStrLn "# Analyses"
  putStrLn "## Constant Propagation"
  pPrintLightBg $ flipMap $ fmap runTotalMap $ secondOf3 $ uncurry mfpSolution' constantPropM

  putStrLn ""
  putStrLn "## Strongly Live Variables"
  pPrintLightBg $ flipMap $ fmap runTotalMap $ secondOf3 $ uncurry mfpSolution' strongLiveM
  --pPrintLightBg $ uncurry mfpSolution' strongLiveM


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
