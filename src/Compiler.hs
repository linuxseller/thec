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
  \  mov rax, 60\n\
  \  mov rdi, 69\n\
  \  syscall\n\n"

addString :: String -> Either String (String, DataSection) -> Either String (String, DataSection)
addString _ (Left x) = Left x
addString str (Right (code, ds)) = Right (str <> code, ds)

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
compile' (AstPrintf:_:(AstString str):_:xs) ds =
  addString
    ("  mov rdi, L"<> (show $ 1 + length ds) <>"\n  call printf\n")
    $ compile' xs (ds <> [str])
compile' (x:xs) ds = compile' xs ds

compile :: [AST] -> Either String String -- left:error & right:code
compile ast = Right $ entryPoint <> code <> dsString
  where (Right (code, ds)) = compile' ast []
        dsString = dsToString ds

dsToString :: DataSection -> String
dsToString ds = unlines $ zipWith3 (\x -> (\y -> (\z -> x <> show y <> ": db \"" <> z <> "\", 0"))) (repeat "L") [1..] ds
