module Main where

import System.Exit
import Control.Applicative
import Control.Monad

import Parser
import Data.Parser
import Data.AST
import BuildAst
import Compiler
import Compilers.X86_64linux

main :: IO ()
main = do
  fileContent <- readFile "testing/test.c"
  let maybeToken = runParser parseFileContent fileContent
  -- print maybeToken
  case maybeToken of
    Nothing -> do
      putStrLn "ERROR PARSING"
      exitWith $ ExitFailure 1
    (Just (x,tokens)) -> if x /= "\n"
      then do
        putStrLn "ERROR PARSING (non empty ast `fst`)"
        exitWith $ ExitFailure 1
      else do
        let aasstt = buildAST tokens
        case compile aasstt of
          (Right ir)-> do
            let assembly = compileIR ir
            writeFile "testing/asmest.asm" assembly
          _ -> putStrLn "Something horribly went wrong somewhere"
  return ()
