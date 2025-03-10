module Compiler where

import Data.Parser

type DataSection = [String] -- only supporting string literals for now

functionStart =
  "  push rbp\n\
  \  mov rbp, rsp\n"
functionEnd =
  "  pop rbp\n"
entryPoint =
  "format ELF64\n\
  \section '.text' executable\n\
  \public _start\n\
  \extrn printf\n\
  \_start:\n\
  \  call main\n\
  \.end:\n\
  \  mov rdi, rax\n\
  \  mov rax, 60\n\
  \  syscall\n\n"

addString :: String -> Either String (String, DataSection) -> Either String (String, DataSection)
addString _ (Left x) = Left x
addString str (Right (code, ds)) = Right (str <> code, ds)

argument_placing =
  [" mov rdi, "
  , " mov rsi, "
  , " mov rdx, "
  , " mov rcx, "
  , " mov r8, "
  , " mov r9, "] <> (repeat "push ")

compileArgumentsPass :: [AST] -> Int -> DataSection -> ([AST], String, DataSection)
compileArgumentsPass (AstParen ')':xs) n_passed ds = (xs, "", ds)
compileArgumentsPass ((AstNum value):xs) n_passed ds = (rest_ast, (argument_placing !! n_passed <> show value) <> args, new_ds)
  where (rest_ast, args, new_ds) = compileArgumentsPass xs (n_passed+1) ds
compileArgumentsPass ((AstString value):xs) n_passed ds = (rest_ast, (argument_placing !! n_passed <> ("L" <> (show $ 1 + (length ds))) <> "\n") <> args, new_ds)
  where (rest_ast, args, new_ds) = compileArgumentsPass xs (n_passed+1) (ds<>[value])
compileArgumentsPass (AstComma:xs) n_passed ds = compileArgumentsPass xs n_passed ds

compile' :: [AST] -> DataSection -> Either String (String, DataSection)
compile' [] ds = Right ("", ds)
compile' ((AstType t):AstMain:(AstParen '('):(AstParen ')'):xs) ds =
  addString
    ("main:\n"<>functionStart)
    $ compile' xs ds
compile' (AstReturn:(AstNum ret_val):AstSemicolon:xs) ds =
  addString
    (functionEnd <> "  mov rax, " <> show ret_val <>"\n  ret\n")
    $ compile' xs ds
compile' (AstReturn:AstSemicolon:xs) ds =
  addString
    ("  ret\n")
    $ compile' xs ds
compile' (AstReturn:_:xs) ds = Left "Returning non-integer literals unsopported"
compile' (AstPrintf:(AstParen '('):xs) ds =
  addString (passed_args <> "\n  call printf\n")
    $ compile' rest_ast new_ds
  where (rest_ast, passed_args, new_ds) = compileArgumentsPass xs 0 ds
compile' (x:xs) ds = compile' xs ds

compile :: [AST] -> Either String String -- left:error & right:code
compile ast = Right $ entryPoint <> code <> dsString
  where (Right (code, ds)) = compile' ast []
        dsString = dsToString ds

dsToString :: DataSection -> String
dsToString ds = unlines $ zipWith3 (\x -> (\y -> (\z -> x <> show y <> ": db \"" <> z <> "\", 0"))) (repeat "L") [1..] ds
