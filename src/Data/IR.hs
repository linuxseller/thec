module Data.IR where

type DataSection = [String]

data GeneralRegister = R1 | R2 | R3 | R4 | R5 | R6 | SP | BP
  deriving Show
data SysCallReg      = SR1 | SR2 | SR3 | SR4 | SR5 | SR6 | SR7
  deriving Show
data FunCallReg      = FR1 | FR2 | FR3 | FR4 | FR5 | FR6 | FOTHER | FRET
  deriving (Enum, Show)   -- FIX
data Register        = SCR SysCallReg | FCR FunCallReg | GR GeneralRegister
  deriving Show
type Intermediate    = Int
data MemPtr          = RawPtr Int | LabelPtr String
  deriving Show
data Operand         = Greg GeneralRegister | Sreg SysCallReg | Freg FunCallReg | NumLit Int | Ptr MemPtr
  deriving Show
data OPSize          = S64 | S32 | S16 | S8
  deriving Show
-- Mov dst src size offset
-- Add dst src size
data Instruction =
  Mov Operand Operand OPSize Int
  | Add Register Operand Int
  | Label String
  | Push Operand
  | Pop Operand
  | Ret -- FIXME
  | Call String
  | Syscall
    deriving Show

