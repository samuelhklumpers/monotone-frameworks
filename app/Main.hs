module Main where

import System.Environment

import Compiler

import Path (splitExtension, parseRelDir, fileExtension)
import Path.IO (listDir)
import Data.List (isSuffixOf)
import Std (throwString, traverse_, catMaybes, readFileUtf8, fromMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["allExamples"] -> allExamples
    [programName] -> do
      let fileName = "./examples/" ++ programName ++ ".c"
      content <- readFile fileName
      compile content
    _ -> do
      putStrLn "Expected name of example program"
      putStrLn "Usage: stack run -- name"

allExamples :: IO ()
allExamples =
  ((=<<) . traverse_) (\(f, c) -> print f *> compile c) $
  ((=<<) . traverse) (\f -> (f, ) <$> readFileUtf8 f) $
  fmap
    (filter
      (
        fromMaybe False .
        fmap not .
        fmap ("~" `isSuffixOf`) .
        fileExtension
      )
    ) $
  fmap snd $
  listDir =<< parseRelDir "examples"
