module Compiler where

import Data.Parser
import Data.CompileAST

functionStart :: [Instruction]
functionEnd :: [Instruction]
entryPoint :: [Instruction]
functionStart = [ Push (Greg BP), Mov (Greg BP) (Greg SP) S64 0]
functionEnd   = [ Pop (Greg BP) ]
entryPoint =
  [Label "_start:"
  , Call "main"
  , Label ".end:"
  , Mov (Sreg SR2) (Freg FRET) S64 0
  , Mov (Sreg SR1) (NumLit 60) S64 0
  , Syscall]

compileArgumentsPass :: [AST] -> Int -> DataSection -> ([AST], [Instruction], DataSection)
compileArgumentsPass (AstParen ')':xs) n_passed ds = (xs, [], ds)
compileArgumentsPass ((AstNum value):xs) n_passed ds =
  (rest_ast, [Mov (Freg (toEnum n_passed :: FunCallReg)) (NumLit value) S64 0] <> args, new_ds)
    where (rest_ast, args, new_ds) = compileArgumentsPass xs (n_passed+1) ds
compileArgumentsPass ((AstString value):xs) n_passed ds =
  (rest_ast,
    [Mov (Freg (toEnum n_passed :: FunCallReg)) (Ptr $ LabelPtr("L" <> (show $ 1 + length ds))) S64 0] <> args,
     new_ds)
    where (rest_ast, args, new_ds) = compileArgumentsPass xs (n_passed+1) (ds<>[value])
compileArgumentsPass (AstComma:xs) n_passed ds = compileArgumentsPass xs n_passed ds

addInstruction :: [Instruction] -> Either String ([Instruction], DataSection) -> Either String ([Instruction], DataSection)
addInstruction _ (Left x) = Left x
addInstruction str (Right (code, ds)) = Right (str <> code, ds)


compile' :: [AST] -> DataSection -> Either String ([Instruction], DataSection)
compile' [] ds = Right ([], ds)
compile' ((AstType t):AstMain:(AstParen '('):(AstParen ')'):xs) ds =
  addInstruction (Label "main:" : functionStart) $ compile' xs ds
compile' (AstReturn:(AstNum ret_val):AstSemicolon:xs) ds =
  addInstruction (functionEnd <> [Mov (Freg FRET) (NumLit ret_val) S64 0, Ret]) $ compile' xs ds
compile' (AstReturn:AstSemicolon:xs) ds = addInstruction [Ret] $ compile' xs ds
compile' (AstReturn:_:xs) ds = Left "Returning non-integer literals unsopported"
compile' (AstPrintf:(AstParen '('):xs) ds = addInstruction (passed_args <> [Call "printf"]) $ compile' rest_ast new_ds
  where (rest_ast, passed_args, new_ds) = compileArgumentsPass xs 0 ds
compile' (x:xs) ds = compile' xs ds

compile :: [AST] -> Either String ([Instruction], DataSection)-- left:error & right:code
compile ast = Right ((entryPoint <> code), ds)
  where (Right (code, ds)) = compile' ast []

