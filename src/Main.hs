module Main where

import System.Exit
import Control.Applicative

import Parser
import Data.Parser
import Compiler

main :: IO ()
main = do
  fileContent <- readFile "testing/test.c"
  let maybeAst = runParser parseFileContent fileContent
  print maybeAst
  case maybeAst of
    Nothing -> do
      putStrLn "ERROR PARSING"
      exitWith $ ExitFailure 1
    (Just (x,ast)) -> if x /= "\n"
      then do
        putStrLn "ERROR PARSING (non empty ast `fst`)"
        exitWith $ ExitFailure 1
      else do
        case compile ast of
          (Right assembly) -> do
            writeFile "testing/asmest.asm" assembly
          (Left error) -> putStrLn $ "[ERROR]" <> error
