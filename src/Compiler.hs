module Compiler where

import Parser
import Lexer
import AttributeGrammar

compile :: String -> IO ()
compile source = do
  let program = happy $ alex source
  let synProgram  = wrap_Program  (sem_Program  program)  Inh_Program
  let program' = labelled_Syn_Program synProgram
  let synProgram' = wrap_Program' (sem_Program' program') Inh_Program'

  putStrLn ""
  putStrLn "# Program"
  putStrLn $ pretty_Syn_Program' synProgram'

  putStrLn ""
  putStrLn "# Output"
  print $ init_Syn_Program' synProgram'
  print $ final_Syn_Program' synProgram'
  print $ flow_Syn_Program' synProgram'
  -- print $ strongLive_Syn_Program' synProgram'
  -- can't print :(