module Main where

import System.Environment

import Compiler

main :: IO ()
main = do
  args <- getArgs
  case args of
    [programName] -> do
      let fileName = "./examples/" ++ programName ++ ".c"
      content <- readFile fileName
      compile content
    _ -> do
      putStrLn "Expected name of example program"
      putStrLn "Usage: stack run -- name"
