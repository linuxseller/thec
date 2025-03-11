module Data.AST where

data AST = IntMainDef | ReturnNum Int | ReturnVoid | FunCall String [AST] | AstString String | AstNum Int
