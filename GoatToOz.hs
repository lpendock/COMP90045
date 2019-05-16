module GoatToOz (compileToOz) where

import GoatAST
import SymTable
import Data.Maybe
import OzTree
import PrintOz (printOzCode)
import GoatAnalyze

-- Change and/or such that if the first expression evaluates false/true you can
-- break the conditional expression

exprToOz :: Int -> Expr -> ExprTable -> SlotStack -> [OzLine]
exprToOz num expr et stack =
  case expr of
    BoolCon _ True
      -> [InstrLine (Int_Const (Reg num) 1)]
    BoolCon _ False
      -> [InstrLine (Int_Const (Reg num) 0)]
    IntCon _ int
      -> [InstrLine (Int_Const (Reg num) int)]
    FloatCon _ float
      -> [InstrLine (Real_Const (Reg num) float)]
    StrCon _ str
      -> [InstrLine (Str_Const (Reg num) str)]
    Id _ ident
      -> let slot = slotFromIdent ident stack in
        case slot of
          Just s -> [InstrLine (Load (Reg num) s)]
          _ -> [InstrLine (Load (Reg num) (Slot 0))] --Should never happen
    ToFloat e1
      -> let instrs = exprToOz num e1 et stack in
        instrs ++ [InstrLine (Int_To_Real (Reg num) (Reg num))]
    And _ e1 e2
      -> 
        let 
          instrs1 = exprToOz num e1 et stack
          instrs2 = exprToOz (num+1) e2 et stack
        in
          instrs1 ++ instrs2 ++ [InstrLine (And_instr (Reg num) (Reg num) 
          (Reg (num+1)))]
    Or _ e1 e2
      -> 
        let 
          instrs1 = exprToOz num e1 et stack
          instrs2 = exprToOz (num+1) e2 et stack
        in
          instrs1 ++ instrs2 ++ 
          [InstrLine (Or_instr (Reg num) (Reg num) (Reg (num+1)))]
    Not _ e1
      -> let instrs = exprToOz num e1 et stack in
        instrs ++ [InstrLine (Not_instr (Reg num) (Reg num))]
    Rel p op e1 e2
      -> let
          instrs1 = exprToOz num e1 et stack
          instrs2 = exprToOz (num+1) e2 et stack
          instrs = instrs1 ++ instrs2
          expr_type = typeFromPos p et
        in
          case op of
            Op_eq
              -> case expr_type of
                  Just FloatType ->
                    instrs ++ 
                    [InstrLine (Cmp_Eq_Real (Reg num) (Reg num) (Reg (num+1)))]
                  _ -> 
                    instrs ++ 
                    [InstrLine (Cmp_Eq_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_ne
              -> case expr_type of
                Just FloatType ->
                  instrs ++ 
                  [InstrLine (Cmp_Ne_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ -> 
                  instrs ++ 
                  [InstrLine (Cmp_Ne_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_gt
              -> case expr_type of
                Just FloatType ->
                  instrs ++ 
                  [InstrLine (Cmp_Gt_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ -> 
                  instrs ++ 
                  [InstrLine (Cmp_Gt_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_ge
              -> case expr_type of
                Just FloatType ->
                  instrs ++ 
                  [InstrLine (Cmp_Ge_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ -> 
                  instrs ++ 
                  [InstrLine (Cmp_Ge_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_le
              -> case expr_type of
                Just FloatType ->
                  instrs ++ 
                  [InstrLine (Cmp_Le_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ -> 
                  instrs ++ 
                  [InstrLine (Cmp_Le_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_lt
              -> case expr_type of
                Just FloatType ->
                  instrs ++ 
                  [InstrLine (Cmp_Lt_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ -> 
                  instrs ++ 
                  [InstrLine (Cmp_Lt_Int (Reg num) (Reg num) (Reg (num+1)))]
    BinOpExp p op e1 e2
      -> let
          instrs1 = exprToOz num e1 et stack
          instrs2 = exprToOz (num+1) e2 et stack
          instrs = instrs1 ++ instrs2
          expr_type = typeFromPos p et
        in
          case op of
            Op_add
              -> case expr_type of
                  Just FloatType 
                    -> instrs ++ 
                      [InstrLine (Add_Real (Reg num) (Reg num) (Reg (num+1)))]
                  _ 
                    -> instrs ++ 
                      [InstrLine (Add_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_sub
              -> case expr_type of
                Just FloatType 
                  -> instrs ++ 
                    [InstrLine (Sub_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ 
                  -> instrs ++ 
                    [InstrLine (Sub_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_mul
              -> case expr_type of
                Just FloatType 
                  -> instrs ++ 
                    [InstrLine (Mul_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ 
                  ->  instrs ++ 
                    [InstrLine (Mul_Int (Reg num) (Reg num) (Reg (num+1)))]
            Op_div
              -> case expr_type of
                Just FloatType 
                  -> instrs ++ 
                    [InstrLine (Div_Real (Reg num) (Reg num) (Reg (num+1)))]
                _ 
                  -> instrs ++ 
                    [InstrLine (Div_Int (Reg num) (Reg num) (Reg (num+1)))]
    UnaryMinus p e1
      -> let
          instrs = exprToOz num e1 et stack
          expr_type = typeFromPos p et
        in
          case expr_type of
            Just FloatType 
              -> instrs ++ [InstrLine (Neg_Real (Reg num) (Reg num))]
            _ 
              -> instrs ++ [InstrLine (Neg_Int (Reg num) (Reg num))]
--  Add arrays

stmtToOz :: Stmt -> ExprTable -> SlotStack -> LookupTable -> Int -> OzCode
stmtToOz stmt et stack lookupTable labelNumber =
  case stmt of
    Assign p lvalue e1
      -> let
          instrs = exprToOz 0 e1 et stack
        in
        case lvalue of
          LId pos ident
            -> let
                slot = slotFromIdent ident stack
              in
                case slot of
                  Just s
                    -> OzCode labelNumber (
                      instrs ++ [InstrLine (Store s (Reg 0))])
                  _
                    -> OzCode labelNumber (
                      instrs ++ [InstrLine (Store (Slot 0) (Reg 0))])
            -- Deal with arrays later
    Read p lvalue 
      -> let 
          lookup_type = getTypeFromLvalue lvalue lookupTable
          slot = slotFromLvalue lvalue stack
        in
          case lvalue of
            LId _ _
              -> case (slot) of
                Just (Slot n)
                  -> case (lookup_type) of
                    Just (_, Base IntType)
                      -> 
                        OzCode labelNumber [InstrLine (Call_Builtin Read_int), 
                        InstrLine (Store (Slot n) (Reg 0))]
                    Just (_, Base FloatType)
                      -> 
                        OzCode labelNumber [InstrLine (Call_Builtin Read_real), 
                        InstrLine (Store (Slot n) (Reg 0))]
                    Just (_, Base BoolType)
                      -> 
                        OzCode labelNumber [InstrLine (Call_Builtin Read_real), 
                        InstrLine (Store (Slot n) (Reg 0))]
                _
                  -> 
                    OzCode labelNumber [InstrLine (Load (Reg 0) (Slot 0)), 
                    InstrLine (Call_Builtin Print_int)]
          -- Deal with arrays later
            _ 
              -> 
                OzCode labelNumber [InstrLine (Load (Reg 0) (Slot 0)), 
                InstrLine (Call_Builtin Print_int)]
    Write p e1
      -> let
          instrs = exprToOz 0 e1 et stack
          expr_type = typeFromExpr e1 et
        in
          case expr_type of
            Just BoolType 
              -> 
                OzCode labelNumber 
                (instrs ++ [InstrLine (Call_Builtin Print_bool)])
            Just IntType 
              -> 
                OzCode labelNumber 
                (instrs ++ [InstrLine (Call_Builtin Print_int)])
            Just FloatType 
              -> 
                OzCode labelNumber 
                (instrs ++ [InstrLine (Call_Builtin Print_real)])
            Just StringType 
              -> 
                OzCode labelNumber 
                (instrs ++ [InstrLine (Call_Builtin Print_string)])
            _ 
              -> 
                OzCode labelNumber 
                (instrs ++ [InstrLine (Call_Builtin Print_int)])
    --Deal with procedures later

    If p e1 stmts
      -> let
          instrs = exprToOz 0 e1 et stack
          OzCode newLabelNum part2 = 
            stmtListToOz stmts [] et stack lookupTable (labelNumber+1)
          part1 = 
            instrs ++ [InstrLine (Branch_On_True (Reg 0) (Label labelNumber)), 
            InstrLine (Branch_Uncond (Label newLabelNum)),
            LabelLine (Label labelNumber)]
          part3 = [LabelLine (Label newLabelNum)]
        in
          OzCode (newLabelNum+1) (part1 ++ part2 ++ part3)

stmtListToOz :: 
  [Stmt] -> [OzLine] -> ExprTable -> SlotStack -> LookupTable -> Int -> OzCode          
stmtListToOz stmts ozlines et stack lookupTable labelNumber =
  case stmts of
    [stmt] 
      -> 
        let 
          OzCode newLabelNum lines = 
            stmtToOz stmt et stack lookupTable labelNumber
          newLines = ozlines ++ lines
        in
          OzCode newLabelNum newLines
          
    (stmt:stmtlist)
      ->
        let
          OzCode newLabelNum lines = 
            stmtToOz stmt et stack lookupTable labelNumber
          newLines = ozlines ++ lines
        in
          stmtListToOz stmtlist newLines et stack lookupTable newLabelNum

programToOz :: Program -> ExprTable -> OzCode
programToOz (Program procedures) et =
  let
    procTable = buildProcTable procedures
    main = searchProcTable "main" procTable
    slotStack = buildStack
  in
    case main of
      Just mainProcedure
        -> procedureToOz mainProcedure et slotStack 0
      _
        -> OzCode 0 []

procedureToOz :: Procedure -> ExprTable -> SlotStack -> Int -> OzCode
procedureToOz (Procedure pos ident args decls stmts) et slotStack labelNumber =
  let
    lookUpTable = buildLookupTable (Procedure pos ident args decls stmts)
    line1 = [ProcLine ident]
    line2 = [InstrLine (Push_Stack_Frame (length decls))]
    line3 = [InstrLine (Int_Const (Reg 0) 0)]
    declLines = initialiseDecls decls 0 []
    (OzCode labelNumber stmtLines) = 
      stmtListToOz stmts [] et slotStack lookUpTable 0
  in
    OzCode labelNumber (line1 ++ line2 ++ line3 ++ declLines ++ stmtLines)

initialiseDecls :: [Decl] -> Int -> [OzLine] -> [OzLine]
initialiseDecls decls num code = 
  case decls of
    [] -> code
    (d:ds) 
      -> 
        initialiseDecls ds (num+1) (code ++ 
        [InstrLine (Store (Slot num) (Reg 0))])

compileToOz :: Program -> String
compileToOz program =
  let 
    ParsedProg newProgram errors et = check_program program 
  in
    case errors of
      [] -> printOzCode (programToOz newProgram et)
      _ -> show errors
