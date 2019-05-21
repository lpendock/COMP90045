module PrintOz (printOzCode) where

import OzTree
import Data.List (intercalate)

printOzCode :: OzCode -> String
printOzCode (OzCode num lines) = concat (map printOzLine lines)

printOzLine :: OzLine -> String
printOzLine line =
  case line of
    LabelLine label -> "\n" ++ (printLabel label)  --fix this
    InstrLine instr -> "\n    " ++ (printInstr instr)

printLabel :: Label -> String
printLabel label =
  case label of
    Label n -> "label_" ++ (show n) ++ ":"
    ProcLabel ident -> "proc_" ++ ident ++ ":"

printSlot :: Slot -> String
printSlot (Slot n) = show n

printReg :: Reg -> String
printReg (Reg n) = "r" ++ (show n)

printInstr :: Instr -> String
printInstr instr =
  case instr of
    Push_Stack_Frame n -> "push_stack_frame " ++ (show n)
    Pop_Stack_Frame n -> "pop_stack_frame " ++ (show n)
        
    Load r0 slot 
      -> "load " ++ (printReg r0) ++ ", " ++ (printSlot slot)             
    Load_Address r0 slot 
      -> "load_address " ++ (printReg r0) ++ ", " ++ (printSlot slot)     
    Load_Indirect r0 r1 
      -> "load_indirect " ++ (intercalate ", " (map printReg [r0,r1]))    
    Store s r0 -> "store " ++ (printSlot s) ++ ", " ++ (printReg r0)            
    Store_Indirect r0 r1 
      -> "store_indirect " ++ (intercalate ", " (map printReg [r0,r1]))    

    Int_Const r0 int -> "int_const " ++ (printReg r0) ++ ", " ++ (show int)           
    Real_Const r0 f -> "real_const " ++ (printReg r0) ++ ", " ++ (show f)
    Str_Const r0 string 
      -> "string_const " ++ (printReg r0) ++ ", " ++ (show string)     

    Add_Int r0 r1 r2 
      -> "add_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))       
    Add_Real r0 r1 r2
      -> "add_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Add_Offset r0 r1 r2
      -> "add_offset " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Sub_Int r0 r1 r2        
      -> "sub_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Sub_Real r0 r1 r2
      -> "sub_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Sub_Offset r0 r1 r2
      -> "sub_offset " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Mul_Int r0 r1 r2
      -> "mul_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Mul_Real r0 r1 r2
      -> "mul_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Div_Int r0 r1 r2
      -> "div_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Div_Real r0 r1 r2
      -> "div_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Neg_Int r0 r1
      -> "neg_int " ++ (intercalate ", " (map printReg [r0,r1]))
    Neg_Real r0 r1
      -> "neg_real " ++ (intercalate ", " (map printReg [r0,r1]))

    Cmp_Eq_Int r0 r1 r2
      -> "cmp_eq_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Ne_Int r0 r1 r2
      -> "cmp_ne_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Gt_Int r0 r1 r2
      -> "cmp_gt_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Ge_Int r0 r1 r2
      -> "cmp_ge_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Lt_Int r0 r1 r2
      -> "cmp_lt_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Le_Int r0 r1 r2
      -> "cmp_le_int " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Eq_Real r0 r1 r2
      -> "cmp_eq_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Ne_Real r0 r1 r2
      -> "cmp_ne_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Gt_Real r0 r1 r2
      -> "cmp_gt_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Ge_Real r0 r1 r2
      -> "cmp_ge_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Lt_Real r0 r1 r2
      -> "cmp_lt_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Cmp_Le_Real r0 r1 r2
      -> "cmp_le_real " ++ (intercalate ", " (map printReg [r0,r1,r2]))

    And_instr r0 r1 r2
      -> "and " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Or_instr r0 r1 r2
      -> "or " ++ (intercalate ", " (map printReg [r0,r1,r2]))
    Not_instr r0 r1
      -> "not " ++ (intercalate ", " (map printReg [r0,r1]))

    Int_To_Real r0 r1 
      -> "int_to_real " ++ (intercalate ", " (map printReg [r0,r1]))
    Move r0 r1 
      -> "move " ++ (intercalate ", " (map printReg [r0,r1]))

    Branch_On_True r0 l 
      -> "branch_on_true " ++ (printReg r0) ++ ", " ++ (printLabel l)
    Branch_On_False r0 l 
      -> "branch_on_false " ++ (printReg r0) ++ ", " ++ (printLabel l)
    Branch_Uncond l 
      -> "branch_uncond " ++ (printLabel l)

    Call_Builtin f -> "call_builtin " ++ (printBuiltin f)
    Call (ProcLabel ident) -> "call " ++ "proc_" ++ ident
    Return -> "return"
    Halt -> "halt"

printBuiltin :: BuiltinFunction -> String
printBuiltin f = 
  case f of
    Read_int -> "read_int"
    Read_real -> "read_real"
    Read_bool -> "read_bool"
    Print_int -> "print_int"
    Print_real -> "print_real"
    Print_bool -> "print_bool"
    Print_string -> "print_string"