import Distribution.Simple
import Distribution.Simple.UserHooks
import System.Process
import System.Exit

main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks{ preBuild = preBuildHook }

preBuildHook args flags = do
  spawnCommand "cd src && stack exec alex -- Lexer.x" >>= waitForProcess >>= checkExit "Lexer.x"
  spawnCommand "cd src && stack exec happy -- Parser.y" >>= waitForProcess >>= checkExit "Parser.y"
  spawnCommand "cd src && stack exec uuagc -- -Hdcfws --self AttributeGrammar.ag --module AttributeGrammar" >>= waitForProcess >>= checkExit "AttributeGrammar.ag"
  preBuild simpleUserHooks args flags

checkExit :: String -> ExitCode -> IO ()
checkExit _ ExitSuccess = return ()
checkExit file _ = error $ "Compilation of " ++ file ++ " failed"
