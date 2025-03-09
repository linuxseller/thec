module Compiler where

import Data.Parser

functionStart =
  "  push rbp\n\
  \  mov rbp, rsp\n"
functionEnd =
  "  pop rbp\n"
entryPoint =
  "format ELF64\n\
  \section '.data' executable\n\
  \hello: db \"BomBom\", 10\n\n\
  \section '.text' executable\n\
  \public _start\n\
  \extrn printf\n\
  \_start:\n\
  \  call main\n\
  \.end:\n\
  \  mov rax, 60\n\
  \  mov rdi, 69\n\
  \  syscall\n\n"

compile' :: [AST] -> Either String String
compile' [] = Right ""
compile' ((AstType t):AstMain:(AstParen '('):(AstParen ')'):xs) = (("main:\n"<>functionStart)<>) <$> compile' xs
compile' (AstReturn:(AstNum ret_val):AstSemicolon:xs) =
  ((functionEnd <> "  mov rax, " <> show ret_val <>"\n  ret\n") <>) <$> compile' xs
compile' (AstReturn:AstSemicolon:xs) = ("  ret\n"<>) <$> compile' xs
compile' (AstReturn:_:xs) = Left "Returning non-integer literals unsopported"
compile' (AstPrintf:_:(AstString str):_:xs) = ("  mov rdi, hello\n  call printf\n"<>) <$> compile' xs
compile' (x:xs) = compile' xs

compile :: [AST] -> Either String String -- left:error & right:code
compile ast = (entryPoint <>) <$> compile' ast

