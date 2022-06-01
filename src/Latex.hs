module Latex where

import Analyses (DifTrans, Edge, Inter (..), lookupR, flipMap)
import AnalysesConversion
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

-- | Format and print an analysis result as a latex array
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

-- | Format a table of properties as a latex array, with the given name and property formatter
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

constantBranchTex :: Maybe ConstBranchLat -> String
constantBranchTex Nothing       = ""
constantBranchTex (Just (c, d)) = pc c ++ "; " ++ pd d
  where
    pc Nothing  = "\\bot"
    pc (Just e) = constEnvTex e

    pd Nothing              = "U"
    pd (Just (Intersect x)) = setTex show x

setTex :: (a -> String) -> Set a -> String
setTex p s
  | S.null s = "\\emptyset"
  | otherwise   = "\\{" ++ intercalate ", " (p <$> S.toList s) ++ "\\}"

strongLiveTex :: Maybe (Set String) -> String
strongLiveTex Nothing  = "\\bot"
strongLiveTex (Just x) = setTex id x
