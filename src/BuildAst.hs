module BuildAst where

import Data.AST
import Data.Parser

collectArguments :: Int -> [Token] -> ([Token], [AST])
collectArguments level (TokenParen ')':xs) = (xs, [])
collectArguments level (TokenNum val:xs) = (rest_tokens, (AstNum val : arguments))
  where (rest_tokens, arguments) = collectArguments (level+1) xs
collectArguments level (TokenString val:xs) = (rest_tokens, (AstString val : arguments))
  where (rest_tokens, arguments) = collectArguments (level+1) xs
collectArguments level (TokenComma:xs) = collectArguments level xs

buildAST :: [Token] -> [AST]
buildAST = build'

build' :: [Token] -> [AST]
build' [] = []
build' ((TokenType t):TokenMain:(TokenParen '('):(TokenParen ')'):xs) = IntMainDef : build' xs
build' (TokenReturn:(TokenNum ret_val):TokenSemicolon:xs) = ReturnNum ret_val : build' xs
build' (TokenReturn:TokenSemicolon:xs) = ReturnVoid : build' xs
build' (TokenReturn:_:xs) = undefined
build' (TokenPrintf:(TokenParen '('):xs) = FunCall "printf" arguments : build' rest
  where (rest, arguments) = collectArguments 1 xs
build' (x:xs) = build' xs


