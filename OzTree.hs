module OzTree where

import GoatAST

--  Somewhat copy-pasted from the lectures. OzLine stores a series of 
--  instructions and labels. OzCode has a list of OzLines, as well as an
--  indicator for the current label-number.

data Reg
  = Reg Int
  deriving (Show, Eq)

data OzLine 
  = LabelLine Label
  | InstrLine Instr
  deriving (Show, Eq)

data Label
  = Label Int
  | ProcLabel Ident
  deriving (Show, Eq)

data BuiltinFunction 
  = Read_int
  | Read_real
  | Read_bool
  | Print_int
  | Print_real
  | Print_bool
  | Print_string
  deriving (Show, Eq)

data Instr
  = Push_Stack_Frame Int
  | Pop_Stack_Frame Int
      
  | Load Reg Slot               --  rI = x
  | Load_Address Reg Slot       --  rI = &x
  | Load_Indirect Reg Reg       --  rI = *rJ
  | Store Slot Reg              --  x = rI
  | Store_Indirect Reg Reg      --  *rI = rJ

  | Int_Const Reg Int            
  | Real_Const Reg Float       
  | Str_Const Reg String       

  | Add_Int Reg Reg Reg        
  | Add_Real Reg Reg Reg
  | Add_Offset Reg Reg Reg
  | Sub_Int Reg Reg Reg        
  | Sub_Real Reg Reg Reg
  | Sub_Offset Reg Reg Reg
  | Mul_Int Reg Reg Reg
  | Mul_Real Reg Reg Reg
  | Div_Int Reg Reg Reg
  | Div_Real Reg Reg Reg
  | Neg_Int Reg Reg
  | Neg_Real Reg Reg

  | Cmp_Eq_Int Reg Reg Reg
  | Cmp_Ne_Int Reg Reg Reg
  | Cmp_Gt_Int Reg Reg Reg
  | Cmp_Ge_Int Reg Reg Reg
  | Cmp_Lt_Int Reg Reg Reg
  | Cmp_Le_Int Reg Reg Reg
  | Cmp_Eq_Real Reg Reg Reg
  | Cmp_Ne_Real Reg Reg Reg
  | Cmp_Gt_Real Reg Reg Reg
  | Cmp_Ge_Real Reg Reg Reg
  | Cmp_Lt_Real Reg Reg Reg
  | Cmp_Le_Real Reg Reg Reg

  | And_instr Reg Reg Reg
  | Or_instr Reg Reg Reg
  | Not_instr Reg Reg

  | Int_To_Real Reg Reg
  | Move Reg Reg

  | Branch_On_True Reg Label
  | Branch_On_False Reg Label
  | Branch_Uncond Label

  | Call_Builtin BuiltinFunction
  | Call Label
  | Return
  | Halt

  deriving (Show, Eq)

data OzCode
  = OzCode Int [OzLine]
  deriving (Show, Eq)

data Slot
  = Slot Int
  deriving (Show, Eq)