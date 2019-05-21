module GoatToOz (compileToOz) where

import GoatAST
import SymTable
import Data.Maybe
import OzTree
import PrintOz (printOzCode)
import GoatAnalyze

-- Change and/or such that if the first expression evaluates false/true you can
-- break the conditional expression

exprToOz :: Int -> Expr -> ExprTable -> SlotStack -> LookupTable -> [OzLine]
exprToOz num expr et stack lookupTable =
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
      -> 
        let 
          slot = slotFromIdent ident stack
          p = getTypeFromIdent ident lookupTable
        in
        case (slot,p) of
          (Just s, Just (parMode,_)) 
            -> 
              case parMode of
                  Val -> [InstrLine (Load (Reg num) s)]
                  Ref 
                    -> 
                      [ InstrLine (Load (Reg num) s)
                      , InstrLine (Load_Indirect (Reg num) (Reg num))
                      ]
          (_,_) -> [InstrLine (Load (Reg num) (Slot 0))] --Should never happen
    ToFloat e1
      -> let instrs = exprToOz num e1 et stack lookupTable in
        instrs ++ [InstrLine (Int_To_Real (Reg num) (Reg num))]
    And _ e1 e2
      -> 
        let 
          instrs1 = exprToOz num e1 et stack lookupTable
          instrs2 = exprToOz (num+1) e2 et stack lookupTable
        in
          instrs1 ++ instrs2 ++ [InstrLine (And_instr (Reg num) (Reg num) 
          (Reg (num+1)))]
    Or _ e1 e2
      -> 
        let 
          instrs1 = exprToOz num e1 et stack lookupTable
          instrs2 = exprToOz (num+1) e2 et stack lookupTable
        in
          instrs1 ++ instrs2 ++ 
          [InstrLine (Or_instr (Reg num) (Reg num) (Reg (num+1)))]
    Not _ e1
      -> let instrs = exprToOz num e1 et stack lookupTable in
        instrs ++ [InstrLine (Not_instr (Reg num) (Reg num))]
    Rel p op e1 e2
      -> let
          instrs1 = exprToOz num e1 et stack lookupTable
          instrs2 = exprToOz (num+1) e2 et stack lookupTable
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
          instrs1 = exprToOz num e1 et stack lookupTable
          instrs2 = exprToOz (num+1) e2 et stack lookupTable
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
          instrs = exprToOz num e1 et stack lookupTable
          expr_type = typeFromPos p et
        in
          case expr_type of
            Just FloatType 
              -> instrs ++ [InstrLine (Neg_Real (Reg num) (Reg num))]
            _ 
              -> instrs ++ [InstrLine (Neg_Int (Reg num) (Reg num))]
    ArrayRef p ident e1
      ->
        let
          instrs = exprToOz num e1 et stack lookupTable
          slot = slotFromIdent ident stack
        in
          case slot of
            Just (Slot s) 
              ->
                instrs ++ 
                  [ InstrLine (Load_Address (Reg (num+1)) (Slot s))
                  , InstrLine (Sub_Offset (Reg num) (Reg (num+1)) (Reg num))
                  , InstrLine (Load_Indirect (Reg num) (Reg num))
                  ]
            _ -> []
    MatrixRef p ident e1 e2
      ->
        let
          instrs1 = exprToOz num e1 et stack lookupTable
          instrs2 = exprToOz num e2 et stack lookupTable
          slot = slotFromIdent ident stack
          lookup = getTypeFromIdent ident lookupTable
        in
          case slot of
            Just (Slot s)
              -> case lookup of
                Just (parMode, Matrix baseType m n)
                  ->
                    instrs1 ++
                      [ InstrLine (Int_Const (Reg (num+1)) n) 
                      , InstrLine (Mul_Int (Reg num) (Reg num) (Reg (num+1)))
                      , InstrLine (Load_Address (Reg (num+1)) (Slot s))
                      , InstrLine (Sub_Offset (Reg num) (Reg (num+1)) (Reg num))
                      , InstrLine (Load_Indirect (Reg num) (Reg num))
                      ]
                _ -> []
            _ -> []

stmtToOz :: 
  Stmt -> ExprTable -> SlotStack -> LookupTable -> Int -> ProcTable -> OzCode
stmtToOz stmt et stack lookupTable labelNumber procTable =
  case stmt of
    Assign p lvalue e1
      -> let
          instrs = exprToOz 0 e1 et stack lookupTable
        in
        case lvalue of
          LId pos ident
            -> let
                slot = slotFromIdent ident stack
                p = getTypeFromIdent ident lookupTable
              in
                case (slot,p) of
                  (Just s, Just (parMode, _))
                    -> 
                      case parMode of
                        Val
                          ->
                            OzCode labelNumber (
                            instrs ++ [InstrLine (Store s (Reg 0))])
                        Ref
                          ->
                            OzCode labelNumber (instrs ++ 
                            [ InstrLine (Load (Reg 1) s) 
                            , InstrLine (Store_Indirect (Reg 1) (Reg 0))
                            ])
                  _
                    -> OzCode 0 []
            -- Deal with arrays later
          LArrayRef p ident expr
            ->
              let
                instrs2 = exprToOz 1 expr et stack lookupTable
                slot = slotFromIdent ident stack
              in
                case slot of
                  Just s
                    ->
                      OzCode labelNumber (instrs ++ instrs2 ++ 
                      [ InstrLine (Load_Address (Reg 2) s)
                      , InstrLine (Sub_Offset (Reg 1) (Reg 2) (Reg 1))
                      ,  InstrLine (Store_Indirect (Reg 1) (Reg 0))
                      ])
                  _
                    ->
                      OzCode 0 []
          LMatrixRef p ident expr1 expr2
            ->
              let
                instrs2 = exprToOz 1 expr1 et stack lookupTable
                instrs3 = exprToOz 2 expr2 et stack lookupTable
                slot = slotFromIdent ident stack
                matrix = getTypeFromIdent ident lookupTable
              in
                case (slot, matrix) of
                  (Just s, Just (_, Matrix _ m n) )
                    ->
                      OzCode labelNumber (instrs ++ instrs2 ++
                      [ InstrLine (Int_Const (Reg 2) n ) 
                      , InstrLine (Mul_Int (Reg 1) (Reg 1) (Reg 2)) 
                      ] ++ instrs3 ++
                      [ InstrLine (Add_Int (Reg 1) (Reg 1) (Reg 2))
                      , InstrLine (Load_Address (Reg 2) s)
                      , InstrLine (Sub_Offset (Reg 1) (Reg 2) (Reg 1))
                      ,  InstrLine (Store_Indirect (Reg 1) (Reg 0))
                      ])
                  _
                    ->
                      OzCode 0 []
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
          instrs = exprToOz 0 e1 et stack lookupTable
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

    ProcCall p ident exprs
      ->
        let
          procedure = searchProcTable ident procTable
          --exprLines = exprsToArgs 0 exprs et stack
          callLine = [InstrLine (Call (ProcLabel ident))]
        in
          --OzCode labelNumber (exprLines ++ callLine)
          case procedure of
            Just (Procedure _ _ args _ _) 
              ->
                let
                  exprLines = exprsToArgs 0 exprs args et stack lookupTable
                in
                  OzCode labelNumber (exprLines ++ callLine)
            _ 
              -> 
                OzCode 0 []

    If p e1 stmts
      -> 
        let
          instrs = exprToOz 0 e1 et stack lookupTable
          OzCode newLabelNum part2 = 
            stmtListToOz stmts [] et stack lookupTable (labelNumber+1) procTable
          part1 = 
            instrs ++ [InstrLine (Branch_On_True (Reg 0) (Label labelNumber)), 
            InstrLine (Branch_Uncond (Label newLabelNum)),
            LabelLine (Label labelNumber)]
          part3 = [LabelLine (Label newLabelNum)]
        in
          OzCode (newLabelNum+1) (part1 ++ part2 ++ part3)
    IfElse p e1 stmts1 stmts2
      ->
        let
          instrs = exprToOz 0 e1 et stack lookupTable

          OzCode newLabel1 part2 = 
            stmtListToOz stmts1 [] et stack lookupTable (labelNumber+1) 
            procTable

          OzCode newLabel2 part4 = 
            stmtListToOz stmts2 [] et stack lookupTable newLabel1 procTable

          part1 =
            instrs ++ [InstrLine (Branch_On_True (Reg 0) (Label labelNumber)), 
            InstrLine (Branch_Uncond (Label newLabel1)),
            LabelLine (Label labelNumber)]

          part3 = 
            [ InstrLine (Branch_Uncond (Label (newLabel2+1)))
            , LabelLine (Label newLabel1)
            ]

          part5 = [LabelLine (Label (newLabel2 + 1))]
        in
          OzCode (newLabel2+1) (part1 ++ part2 ++ part3 ++ part4 ++ part5)  
    While p e1 stmts
      ->
        let
          instrs = exprToOz 0 e1 et stack lookupTable

          OzCode newLabelNum loopPart = 
            stmtListToOz stmts [] et stack lookupTable (labelNumber+2) procTable
            
          part2 = loopPart ++ [InstrLine (Branch_Uncond (Label (labelNumber+1)))]

          part1 = 
            [LabelLine (Label (labelNumber+1))] ++ instrs ++ 
            [InstrLine (Branch_On_True (Reg 0) (Label newLabelNum)), 
            InstrLine (Branch_Uncond (Label (newLabelNum+1))),
            LabelLine (Label newLabelNum)]

          part3 = [LabelLine (Label (newLabelNum+1))]
        in
          OzCode (newLabelNum+1) (part1 ++ part2 ++ part3)


exprsToArgs :: 
  Int -> [Expr] -> [FormalArgSpec] -> ExprTable -> SlotStack -> LookupTable -> 
    [OzLine]
exprsToArgs n exprs ((FormalArgSpec p parmode b i):args) et slotStack lt =
  case exprs of
    [e]
      -> 
        case parmode of
          Val
            ->
              (exprToOz n e et slotStack lt)
          Ref
            ->
              case e of
                Id _ ident
                  ->
                    let
                      s = slotFromIdent ident slotStack
                    in
                      case s of
                        Just (Slot n)
                          -> 
                            [InstrLine (Load_Address (Reg n) (Slot n))]
                        _
                          -> []
                _
                  -> [] 
    (e:es) 
      ->
        exprsToArgs n [e] ((FormalArgSpec p parmode b i):args) et slotStack lt
        ++ (exprsToArgs (n+1) es args et slotStack lt)
    [] -> []


stmtListToOz :: 
  [Stmt] -> [OzLine] -> ExprTable -> SlotStack -> LookupTable -> Int -> 
  ProcTable -> OzCode          
stmtListToOz stmts ozlines et stack lookupTable labelNumber procTable =
  case stmts of
    [stmt] 
      -> 
        let 
          OzCode newLabelNum lines = 
            stmtToOz stmt et stack lookupTable labelNumber procTable
          newLines = ozlines ++ lines
        in
          OzCode newLabelNum newLines
          
    (stmt:stmtlist)
      ->
        let
          OzCode newLabelNum lines = 
            stmtToOz stmt et stack lookupTable labelNumber procTable
          newLines = ozlines ++ lines
        in
          stmtListToOz stmtlist newLines et stack lookupTable newLabelNum procTable

programToOz :: Program -> ExprTable -> OzCode
programToOz (Program procedures) et =
  let
    procTable = buildProcTable procedures
    OzCode label code = procedureListToOz procedures (OzCode 0 []) et procTable
  in
    OzCode label ([InstrLine (Call (ProcLabel "main")), InstrLine Halt] ++ code)

procedureListToOz :: [Procedure] -> OzCode -> ExprTable -> ProcTable -> OzCode
procedureListToOz procs (OzCode label lines) et procTable =
  case procs of
    [] 
      -> 
        (OzCode label lines)
    (x:xs) 
      -> 
        let 
          OzCode newLabel newLines = procedureToOz x et label procTable 
        in
          procedureListToOz xs (OzCode newLabel (lines ++ newLines)) et 
            procTable

procedureToOz :: Procedure -> ExprTable -> Int -> ProcTable -> OzCode
procedureToOz (Procedure pos ident args decls stmts) et labelNumber procTable =
  let
    slotStack = addDeclsToStack decls (addArgsToStack args (buildStack))
    lookUpTable = buildLookupTable (Procedure pos ident args decls stmts)
    SlotStack size _ = slotStack
    line1 = [LabelLine (ProcLabel ident)]
    line2 = [InstrLine (Push_Stack_Frame (size))]
    argsLines = argsToCode 0 args slotStack
    declLines = initialiseDecls decls 0 []
    (OzCode labelNumber stmtLines) = 
      stmtListToOz stmts [] et slotStack lookUpTable 0 procTable
    lastLines = [InstrLine (Pop_Stack_Frame size), InstrLine Return]
  in
    OzCode labelNumber (line1 ++ line2 ++ argsLines ++ declLines ++ 
    stmtLines ++ lastLines)

argsToCode :: Int -> [FormalArgSpec] -> SlotStack -> [OzLine]
argsToCode n args stack =
  case args of
    [] -> []
    (FormalArgSpec _ parmode basetype ident):xs ->
      let
        slot = slotFromIdent ident stack
      in
        case slot of
          Just s -> InstrLine ((Store s (Reg n))):(argsToCode (n+1) xs stack)
          Nothing -> []

initialiseDecls :: [Decl] -> Int -> [OzLine] -> [OzLine]
initialiseDecls decls num code = 
  case decls of
    [] -> code
    ((Decl _ _ goatType):ds) 
      ->
        case goatType of
          Base baseType
            ->
              let
                register = case baseType of
                  IntType -> [InstrLine (Int_Const (Reg 0) 0)]
                  FloatType -> [InstrLine (Real_Const (Reg 0) 0)]
                  BoolType -> [InstrLine (Int_Const (Reg 0) 0)]
              in
              initialiseDecls ds (num+1) (code ++ register ++
                [InstrLine (Store (Slot num) (Reg 0))])
          Array baseType n
            ->
              let
                register = case baseType of
                  IntType -> [InstrLine (Int_Const (Reg 0) 0)]
                  FloatType -> [InstrLine (Real_Const (Reg 0) 0)]
                  BoolType -> [InstrLine (Int_Const (Reg 0) 0)]
              in
              initialiseDecls ds (num+n) 
                (code ++ register ++ initialiseShape num n [])
          Matrix baseType m n
            ->
              let
                register = case baseType of
                  IntType -> [InstrLine (Int_Const (Reg 0) 0)]
                  FloatType -> [InstrLine (Real_Const (Reg 0) 0)]
                  BoolType -> [InstrLine (Int_Const (Reg 0) 0)]
              in
              initialiseDecls ds (num+(n*m)) (code ++ register ++ 
                initialiseShape num (n*m) [])

initialiseShape :: Int -> Int -> [OzLine] -> [OzLine]
initialiseShape start n code =
  if n == 0 
    then code
    else 
      initialiseShape start (n-1) (
        [InstrLine (Store (Slot (start + n - 1)) (Reg 0))] ++ code)

compileToOz :: Program -> String
compileToOz program =
  let 
    ParsedProg newProgram errors et = check_program program 
  in
    case errors of
      [] -> printOzCode (programToOz newProgram et)
      _ -> show errors
