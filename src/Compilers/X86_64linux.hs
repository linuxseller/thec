module Compilers.X86_64linux where

-- SUPPORTING ONLY 64 BIT OPERANDS FOR NOW

import Data.CompileAST

entryPoint =
  "format ELF64\n\
  \section '.text' executable\n\
  \public _start\n\
  \extrn printf\n"

addString :: String -> Either String (String, DataSection) -> Either String (String, DataSection)
addString _ (Left x) = Left x
addString str (Right (code, ds)) = Right (str <> code, ds)

compileIR' :: [Instruction] -> String -- use Either error code
compileIR' [] = []
compileIR' ((Mov dst src S64 offset):xs) =
  ("  mov " <> operandToAsm dst <> ", " <> operandToAsm src <> "\n") <> compileIR' xs
compileIR' ((Call x):xs) = "  call " <> x <> "\n"<> compileIR' xs
compileIR' ((Syscall):xs) = "  syscall\n"<> compileIR' xs
compileIR' ((Label x):xs) = x <> "\n"<> compileIR' xs
compileIR' ((Ret):xs) = "  ret\n"<> compileIR' xs
compileIR' ((Push x):xs) = "  push " <> operandToAsm x <> "\n"<> compileIR' xs
compileIR' ((Pop x):xs) = "  pop " <> operandToAsm x <> "\n"<> compileIR' xs

compileIR (insts, ds) = entryPoint <> compileIR' insts <> dsToString ds

operandToAsm :: Operand -> String
operandToAsm (Greg x) = getRegister x
operandToAsm (Freg x) = getFRegister x
operandToAsm (Sreg x) = getSRegister x
operandToAsm (NumLit x) = show x
operandToAsm (Ptr (LabelPtr x)) = x

getSRegister :: SysCallReg -> String
getSRegister SR1 = "rax"
getSRegister SR2 = "rdi"
getSRegister SR3 = "rsi"
getSRegister SR4 = "rdx"
getSRegister SR5 = "r10"
getSRegister SR6 = "r8"
getSRegister SR7 = "r9"

getFRegister :: FunCallReg -> String
getFRegister FR1 = "rdi"
getFRegister FR2 = "rsi"
getFRegister FR3 = "rdx"
getFRegister FR4 = "rcx"
getFRegister FR5 = "r8"
getFRegister FR6 = "r9"
getFRegister FRET = "rax"

getRegister :: GeneralRegister -> String
getRegister R1 = "rax"
getRegister R2 = "rbx"
getRegister R3 = "rcx"
getRegister R4 = "rdx"
getRegister SP = "rsp"
getRegister BP = "rbp"

dsToString :: DataSection -> String
dsToString ds = unlines $ zipWith3 (\x -> (\y -> (\z -> x <> show y <> ": db \"" <> z <> "\", 0"))) (repeat "L") [1..] ds
