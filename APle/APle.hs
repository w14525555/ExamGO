-- Top-level interactive loop with line editing
-- To run from within GHCi, e.g.,  :set args arithmetic.op rules.ap

import AST
import Syntax
import Semantics

import Control.Monad.RWS
import System.Console.Haskeline
import System.Environment (getArgs)
import Data.List (intercalate)

type APle = RWST OpTable () [Rule] IO 

processFileCmds :: FilePath -> APle ()
processFileCmds fp =
  do txt <- lift $ readFile fp
     opt <- ask
     case parseStringCmds opt txt of
       Right cs -> mapM_ (\c -> state (processCmd c)) cs
       Left e -> error $ "Error in initial rule set " ++ fp ++ "\n" ++ e

processString :: String -> APle ()
processString s =
  do opt <- ask
     case parseStringCmds opt s of
       Left e -> lPutStrLn e
       Right [] -> return ()
       Right [c] ->
         do Rsp ts mmsg <- state (processCmd c)
            lPutStrLn $ intercalate " =\n" (map (printTerm opt) ts)
            case mmsg of
               Nothing -> return ()
               Just m -> lPutStrLn $ "Error: " ++ m
       _ -> lPutStrLn "One at a time, please!"
    where lPutStrLn = lift . putStrLn

repl :: InputT APle ()
repl =
  do ml <- getInputLine "> "
     case ml of
       Nothing -> return ()
       Just s -> do lift $ processString s; repl

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> putStrLn "Usage: APle operators.op rules1.ap ... rulesn.ap"
       (ofp:rfps) ->
         do optxt <- readFile ofp
            let opt = read optxt
            _ <- runRWST (do mapM_ processFileCmds rfps
                             runInputT defaultSettings repl) opt []
            return ()
