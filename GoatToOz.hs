module GoatToOz (compileToOz) where

import GoatAST
import SymTable
import Data.Maybe
import OzTree
import PrintOz (printOzCode)
import GoatAnalyze

--  This is some dreadful code. In hindsight, I would have constructed it very
--  differently, but initially, I didn't have a very good idea of what I was
--  doing and what helper functions/data-structures were necessary.
--  Additionally, I was initially unfamiliar with some of the conventions of 
--  Haskell, which could have prevented some duplicate code, but it's there now 
--  and I don't really feel like, or have the time to fix it.
--
--  In short, I apologise for this mess.



--  Convert an expression represented by the type Expr from GoatAST to the type
--  OzCode, which contains a list of lines of Oz code, and the label-number as
--  of the final line in the generated code. Returns OzCode 0 [] in the case of
--  an error, but the module GoatAnalyze should pick up compile-time errors.

exprToOz :: Int             --  This is the number for the left-hand register 
            -> Expr         --  The expression to be converted.
            -> ExprTable    --  A lookup-table giving the types of each 
                            --  expression.
            -> SlotStack    --  A look-up table giving the slot-number for each
                            --  variable.
            -> LookupTable  --  A look-up table giving the types of each 
                            --  variable.
            -> Int          --  The label-number.
            -> OzCode       --  The generated code.
exprToOz num expr et stack lookupTable labelNumber =
  case expr of
    --  Constants
    BoolCon _ True
      -> OzCode labelNumber [InstrLine (Int_Const (Reg num) 1)]
    BoolCon _ False
      -> OzCode labelNumber [InstrLine (Int_Const (Reg num) 0)]
    IntCon _ int
      -> OzCode labelNumber [InstrLine (Int_Const (Reg num) int)]
    FloatCon _ float
      -> OzCode labelNumber [InstrLine (Real_Const (Reg num) float)]
    StrCon _ str
      -> OzCode labelNumber [InstrLine (Str_Const (Reg num) str)]

    --  Search the lookup table for slots to find the correct slot-number for
    --  the variable. Check if the variable is a value or a reference.
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
                  Val -> OzCode labelNumber [InstrLine (Load (Reg num) s)]
                  Ref 
                    -> 
                      OzCode labelNumber 
                      [ InstrLine (Load (Reg num) s)
                      , InstrLine (Load_Indirect (Reg num) (Reg num))
                      ]
          (_,_) -> OzCode 0 []
    ToFloat e1
      -> 
        let 
          OzCode newLabel instrs = 
            exprToOz num e1 et stack lookupTable labelNumber
        in
        OzCode newLabel (
          instrs ++ [InstrLine (Int_To_Real (Reg num) (Reg num))])
    --  This doesn't seem like a great way of doing it, but I wanted something a
    --  bit more generic than the generated code from tutorial 8 that seemed
    --  specific to if statements, so I modified it to give a constant.
    And _ e1 e2
      -> 
        let 
          OzCode newLabel1 instrs1 = 
            exprToOz num e1 et stack lookupTable labelNumber
          OzCode newLabel2 instrs2 = 
            exprToOz (num+1) e2 et stack lookupTable newLabel1
        in
          OzCode (newLabel2+4) (
            instrs1 ++
            [ InstrLine (Branch_On_False (Reg num) (Label (newLabel2+1)) )
            , InstrLine (Branch_Uncond (Label (newLabel1)) )
            , LabelLine (Label (newLabel1))
            ]
            ++ instrs2 ++
            [ InstrLine (Branch_On_False (Reg num) (Label (newLabel2+1)) )
            , InstrLine (Branch_Uncond (Label (newLabel2+2)) )
            , LabelLine (Label (newLabel2+1))
            , InstrLine (Int_Const (Reg num) 0)
            , InstrLine (Branch_Uncond (Label (newLabel2+3)) )
            , LabelLine (Label (newLabel2+2))
            , InstrLine (Int_Const (Reg num) 1)
            , InstrLine (Branch_Uncond (Label (newLabel2+3)) )
            , LabelLine (Label (newLabel2+3))
            ]
          )
    --  Copy-pasted from above, modifying the expression breaks.
    Or _ e1 e2
      -> 
        let 
          OzCode newLabel1 instrs1 = 
            exprToOz num e1 et stack lookupTable labelNumber
          OzCode newLabel2 instrs2 = 
            exprToOz (num+1) e2 et stack lookupTable newLabel1
        in
          OzCode (newLabel2+4) (
            instrs1 ++
            [ InstrLine (Branch_On_True (Reg num) (Label (newLabel2+1)) )
            , InstrLine (Branch_Uncond (Label (newLabel1)) )
            , LabelLine (Label (newLabel1))
            ]
            ++ instrs2 ++
            [ InstrLine (Branch_On_True (Reg num) (Label (newLabel2+1)) )
            , InstrLine (Branch_Uncond (Label (newLabel2+2)) )
            , LabelLine (Label (newLabel2+1))
            , InstrLine (Int_Const (Reg num) 1)
            , InstrLine (Branch_Uncond (Label (newLabel2+3)) )
            , LabelLine (Label (newLabel2+2))
            , InstrLine (Int_Const (Reg num) 0)
            , InstrLine (Branch_Uncond (Label (newLabel2+3)) )
            , LabelLine (Label (newLabel2+3))
            ]
          )
    Not _ e1
      -> 
        let 
          OzCode newLabel instrs = 
            exprToOz num e1 et stack lookupTable labelNumber 
        in
          OzCode newLabel (
            instrs ++ [InstrLine (Not_instr (Reg num) (Reg num))])
    --  Yuck. Should have found some way of generalizing function, rather than
    --  this...
    Rel p op e1 e2
      -> let
          OzCode newLabel1 instrs1 = 
            exprToOz num e1 et stack lookupTable labelNumber
          OzCode newLabel2 instrs2 = 
            exprToOz (num+1) e2 et stack lookupTable labelNumber
          instrs = instrs1 ++ instrs2
          expr_type = typeFromPos p et
          instrs3 = 
            case op of
              Op_eq
                -> case expr_type of
                    Just FloatType ->
                      instrs ++ 
                      [InstrLine (Cmp_Eq_Real (Reg num) (Reg num) 
                      (Reg (num+1)))]
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
        in
          OzCode newLabel2 instrs3
    --  Copied and pasted from the copy-pastes. 
    BinOpExp p op e1 e2
      -> 
        let
          OzCode newLabel1 instrs1 = 
            exprToOz num e1 et stack lookupTable labelNumber
          OzCode newLabel2 instrs2 = 
            exprToOz (num+1) e2 et stack lookupTable newLabel1
          expr_type = typeFromPos p et
          instrs = instrs1 ++ instrs2
          instrs3 =
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
        in
          OzCode newLabel2 instrs3
    --  Check if the expression is an int or a real.  
    UnaryMinus p e1
      -> let
          OzCode newLabel instrs = 
            exprToOz num e1 et stack lookupTable labelNumber
          expr_type = typeFromPos p et
        in
          case expr_type of
            Just FloatType 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Neg_Real (Reg num) (Reg num))])
            _ 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Neg_Int (Reg num) (Reg num))])
    --  Find the slot-number for the array. Since the arrays and matrices all
    --  seem to be references like in C, use load-address instead of load.
    --  Get the offset from the inner expression, and load the value.
    ArrayRef p ident e1
      ->
        let
          OzCode newLabel instrs = 
            exprToOz num e1 et stack lookupTable labelNumber
          slot = slotFromIdent ident stack
        in
          case slot of
            Just (Slot s) 
              ->
                OzCode newLabel (
                  instrs ++ 
                  [ InstrLine (Load_Address (Reg (num+1)) (Slot s))
                  , InstrLine (Sub_Offset (Reg num) (Reg (num+1)) (Reg num))
                  , InstrLine (Load_Indirect (Reg num) (Reg num))
                  ]
                )
            _ -> OzCode 0 []
    --  Essentially the same as above, but you need to get dimensions m,n of the
    --  matrix, so you can multiply expression 1 by n and add expression 2 to
    --  get the offset.
    MatrixRef p ident e1 e2
      ->
        let
          OzCode newLabel1 instrs1 = 
            exprToOz num e1 et stack lookupTable labelNumber
          OzCode newLabel2 instrs2 = 
            exprToOz num e2 et stack lookupTable newLabel1
          slot = slotFromIdent ident stack
          lookup = getTypeFromIdent ident lookupTable
        in
          case slot of
            Just (Slot s)
              -> case lookup of
                Just (parMode, Matrix baseType m n)
                  ->
                    OzCode newLabel2 (
                      instrs1 ++
                      [ InstrLine (Int_Const (Reg (num+1)) n) 
                      , InstrLine (Mul_Int (Reg num) (Reg num) (Reg (num+1)))
                      , InstrLine (Load_Address (Reg (num+1)) (Slot s))
                      , InstrLine (Sub_Offset (Reg num) (Reg (num+1)) (Reg num))
                      , InstrLine (Load_Indirect (Reg num) (Reg num))
                      ])
                _ -> OzCode 0 []
            _ -> OzCode 0 []


--  Like exprToOz, this function is designed to transform an AST (of a 
--  statement this time) to a representation of Oz code. 
stmtToOz :: Stmt            --  The statement to be converted 
            -> ExprTable    --  A symbol-table giving the types of each 
                            --  expression
            -> SlotStack    --  A symbol-table giving the slot-number of each
                            --  variable
            -> LookupTable  --  A symbol-table giving the type of each variable
            -> Int          --  The label-number 
            -> ProcTable    --  A symbol-table giving the arguments of the 
                            --  procedures
            -> OzCode       --  The generated code.
stmtToOz stmt et stack lookupTable labelNumber procTable =
  case stmt of
    --  Evaluate the expression, and keep the value in register 0. The 
    --  identifier can be a value, an array, or a matrix. In the case of a 
    --  value, you can simply store the value in the corresponding slot. For
    --  the other two types, we must calculate the offset, and use 
    --  store_indirect since they are references.
    Assign p lvalue e1
      -> let
          OzCode newLabel instrs = 
            exprToOz 0 e1 et stack lookupTable labelNumber
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
                            OzCode newLabel (
                            instrs ++ [InstrLine (Store s (Reg 0))])
                        Ref
                          ->
                            OzCode newLabel (instrs ++ 
                            [ InstrLine (Load (Reg 1) s) 
                            , InstrLine (Store_Indirect (Reg 1) (Reg 0))
                            ])
                  _
                    -> OzCode 0 []
          LArrayRef p ident expr
            ->
              let
                OzCode newLabel2 instrs2 = 
                  exprToOz 1 expr et stack lookupTable newLabel
                slot = slotFromIdent ident stack
              in
                case slot of
                  Just s
                    ->
                      OzCode newLabel2 (instrs ++ instrs2 ++ 
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
                OzCode newLabel2 instrs2 = 
                  exprToOz 1 expr1 et stack lookupTable newLabel
                OzCode newLabel3 instrs3 = 
                  exprToOz 2 expr2 et stack lookupTable newLabel2
                slot = slotFromIdent ident stack
                matrix = getTypeFromIdent ident lookupTable
              in
                case (slot, matrix) of
                  (Just s, Just (_, Matrix _ m n) )
                    ->
                      OzCode newLabel3 (instrs ++ instrs2 ++
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
    --  We need to find the type of the identifier and use the corresponding
    --  built-in function. Depending on whether the value is a simple variable
    --  or an array/matrix, store register 0 directly or indirectly via the
    --  calculated offset.
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
                        OzCode labelNumber [InstrLine (Call_Builtin Read_bool), 
                        InstrLine (Store (Slot n) (Reg 0))]
                _
                  -> 
                    OzCode labelNumber [InstrLine (Load (Reg 0) (Slot 0)), 
                    InstrLine (Call_Builtin Print_int)]
            LArrayRef _ ident e1 
              ->
                let
                  OzCode newLabel instrs = 
                    exprToOz 1 e1 et stack lookupTable labelNumber
                  read_line =
                    case lookup_type of
                      Just (_, Array base_type _)
                        ->
                          case base_type of
                            IntType -> [ InstrLine (Call_Builtin Read_int) ]
                            FloatType -> [ InstrLine (Call_Builtin Read_real) ]
                            BoolType -> [ InstrLine (Call_Builtin Read_bool) ]
                      _ -> []
                in
                  case slot of
                    Just s
                      ->
                        OzCode newLabel (
                        read_line ++ instrs ++
                        [ InstrLine (Load_Address (Reg 2) s)
                        , InstrLine (Sub_Offset (Reg 1) (Reg 2) (Reg 1))
                        , InstrLine (Store_Indirect (Reg 1) (Reg 0))
                        ])
                    _ -> OzCode 0 []
            LMatrixRef _ _ e1 e2
              ->
                let
                  OzCode newLabel1 instrs1 = 
                    exprToOz 1 e1 et stack lookupTable labelNumber
                  OzCode newLabel2 instrs2 = 
                    exprToOz 2 e2 et stack lookupTable newLabel1
                  read_line =
                    case lookup_type of
                      Just (_, Matrix base_type m n)
                        ->
                          case base_type of
                            IntType -> [ InstrLine (Call_Builtin Read_int) ]
                            FloatType -> [ InstrLine (Call_Builtin Read_real) ]
                            BoolType -> [ InstrLine (Call_Builtin Read_bool) ]
                      _ -> []
                  n = 
                    case lookup_type of
                      Just (_, Matrix base_type a b) -> b
                      _ -> 0
                in
                  case slot of
                    Just s
                      ->
                        OzCode newLabel2 (
                        read_line ++ instrs1 ++
                        [ InstrLine (Int_Const (Reg 2) n )
                        , InstrLine (Mul_Int (Reg 1) (Reg 1) (Reg 2))
                        ] ++ instrs2 ++
                        [ InstrLine (Add_Int (Reg 1) (Reg 1) (Reg 2))
                        , InstrLine (Load_Address (Reg 2) s)
                        , InstrLine (Store_Indirect (Reg 1) (Reg 0))
                        ])
                    _ -> OzCode 0 []
    --  Get the type of the expression from the expression-type symbol-table,
    --  and use the corresponding built-in function to print it.
    Write p e1
      -> let
          OzCode newLabel instrs = 
            exprToOz 0 e1 et stack lookupTable labelNumber
          expr_type = typeFromExpr e1 et
        in
          case expr_type of
            Just BoolType 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Call_Builtin Print_bool)])
            Just IntType 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Call_Builtin Print_int)])
            Just FloatType 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Call_Builtin Print_real)])
            Just StringType 
              -> 
                OzCode newLabel 
                (instrs ++ [InstrLine (Call_Builtin Print_string)])
            _ 
              -> 
                OzCode 0 []
    --  Need to get the arguments of the procedure represented by the identifer,
    --  and store their values on registers. Then, call the procedure.
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
                  OzCode newLabel exprLines = 
                    exprsToArgs 0 exprs args et stack lookupTable (OzCode 0 [])
                in
                  OzCode newLabel (exprLines ++ callLine)
            _ 
              -> 
                OzCode 0 []
    --  Create a label for the body of the if-statement. If the expression gives
    --  true, branch to the label. Another label will be branched to by both the
    --  initial and second label.
    If p e1 stmts
      -> 
        let
          OzCode newLabel instrs = 
            exprToOz 0 e1 et stack lookupTable labelNumber
          OzCode newLabel2 part2 = 
            stmtListToOz stmts [] et stack lookupTable (newLabel+1) procTable
          part1 = 
            instrs ++ [InstrLine (Branch_On_True (Reg 0) (Label newLabel)), 
            InstrLine (Branch_Uncond (Label newLabel2)),
            LabelLine (Label newLabel)]
          part3 = [InstrLine (Branch_Uncond (Label newLabel2))]
          part4 = [LabelLine (Label newLabel2)]
        in
          OzCode (newLabel2+1) (part1 ++ part2 ++ part3 ++ part4)
    --  Create a second label containing the statements in the else-part of the
    --  statment. The first and second labels will branch to a third label,
    --  representing the rest of the code.
    IfElse p e1 stmts1 stmts2
      ->
        let
          OzCode newLabel1 instrs = 
            exprToOz 0 e1 et stack lookupTable labelNumber

          OzCode newLabel2 part2 = 
            stmtListToOz stmts1 [] et stack lookupTable (newLabel1+1) 
            procTable

          OzCode newLabel3 part4 = 
            stmtListToOz stmts2 [] et stack lookupTable newLabel2 procTable

          part1 =
            instrs ++ [InstrLine (Branch_On_True (Reg 0) (Label newLabel1)), 
            InstrLine (Branch_Uncond (Label newLabel2)),
            LabelLine (Label newLabel1)]

          part3 = 
            [ InstrLine (Branch_Uncond (Label (newLabel3+1)))
            , LabelLine (Label newLabel2)
            ]

          part5 = 
            [ InstrLine (Branch_Uncond (Label (newLabel3 + 1)))
            , LabelLine (Label (newLabel3 + 1))]
        in
          OzCode (newLabel3+1) (part1 ++ part2 ++ part3 ++ part4 ++ part5)
    --  Create a label that represents the condition-check. If the expression
    --  is true, branch to another label that acts as the statement body. Upon
    --  completion, return to the condition-check label. On failure, move to
    --  a third label, which is the rest of the procedure.  
    While p e1 stmts
      ->
        let
          OzCode newLabel1 instrs = 
            exprToOz 0 e1 et stack lookupTable labelNumber

          OzCode newLabel2 loopPart = 
            stmtListToOz stmts [] et stack lookupTable (newLabel1+2) procTable
            
          part2 = 
            loopPart ++ [InstrLine (Branch_Uncond (Label (newLabel1+1)))]

          part1 = 
            [LabelLine (Label (newLabel1+1))] ++ instrs ++ 
            [InstrLine (Branch_On_True (Reg 0) (Label newLabel2)), 
            InstrLine (Branch_Uncond (Label (newLabel2+1))),
            LabelLine (Label newLabel2)]

          part3 = [LabelLine (Label (newLabel2+1))]
        in
          OzCode (newLabel2+1) (part1 ++ part2 ++ part3)

--  This is a helper function for ProcCall. It transforms a list of expressions
--  and procedure arguments into a series of oz instructions. It checks if the
--  argument is a value or a reference, and stores the value or loads the
--  reference to a corresponding register.
exprsToArgs ::  Int                 --  The register number.
                -> [Expr]           --  The expressions given as arguments.
                -> [FormalArgSpec]  --  The arguments of the procedure.
                -> ExprTable        --  The types of each expression.
                -> SlotStack        --  The slot-numbers of each identifier.
                -> LookupTable      --  The types of each identifier.
                -> OzCode           --  The code passed to the next argument in 
                                    --  the list.
                -> OzCode           --  The outputed Oz code.
exprsToArgs n exprs ((FormalArgSpec p parmode b i):args) et slotStack lt code =
  let
    OzCode ln lines = code
  in
  case exprs of
    [e]
      -> 
        case parmode of
          Val
            ->
              let
                OzCode ln2 code2 = exprToOz n e et slotStack lt ln
                exprType = typeFromExpr e et
                lines2 =
                  case exprType of
                    Just etype
                      ->
                        case (b, etype) of
                          (FloatType, IntType) 
                            -> [InstrLine (Int_To_Real (Reg n) (Reg n))]
                          _ -> []
                    _ -> []
              in
                OzCode ln2 (lines ++ code2 ++ lines2)
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
                            OzCode ln (
                              lines ++ 
                              [InstrLine (Load_Address (Reg n) (Slot n))]
                            )
                        _
                          -> OzCode 0 []
                _
                  -> OzCode 0 [] 
    (e:es) 
      ->
        let
          c = 
            exprsToArgs n [e] ((FormalArgSpec p parmode b i):args) 
            et slotStack lt code
        in 
          exprsToArgs (n+1) es args et slotStack lt c
    [] -> OzCode 0 []

--  In hindsight, I should have made this take OzCode as an argument rather than
--  a list of OzLines and the label-number. Transforms a list of statements to
--  a representation of Oz code.
stmtListToOz :: [Stmt]          --  A list of statements.
                -> [OzLine]     --  Lines of Oz code. To be passed along the 
                                --  list. Should initally be empty.
                -> ExprTable    --  The types of each expression.
                -> SlotStack    --  The slot-number of each identifier.
                -> LookupTable  --  The type of each identifier.
                -> Int          --  The label-number.
                -> ProcTable    --  The arguments of each procedure.
                -> OzCode       --  The generated OzCode .  
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
          stmtListToOz stmtlist newLines et stack lookupTable newLabelNum 
            procTable

--  Transform a series of procedures into a representation of Oz code. Start by
--  writing "call proc_main" and "halt", then transforming each procedure.
programToOz ::  Program       --  The program getting transformed.
                -> ExprTable  --  The types of each expression.
                -> OzCode     --  The output code.
programToOz (Program procedures) et =
  let
    procTable = buildProcTable procedures
    OzCode label code = procedureListToOz procedures (OzCode 0 []) et procTable
  in
    OzCode label ([InstrLine (Call (ProcLabel "main")), InstrLine Halt] ++ code)

--  Transform a list of procedures into a representation of Oz code.
procedureListToOz ::  [Procedure]   --  The list of procedures to be tranformed.
                      -> OzCode     --  A representation of Oz code, passed and 
                                    --  built on by each element in the list.
                      -> ExprTable  --  The types of each expression in the 
                                    --  program.
                      -> ProcTable  --  The arguments of the procedures.
                      -> OzCode     --  The output code.
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

--  Transform a procedure into a representation of Oz code. Build the symbol
--  tables necessary for statementToOz and others.
procedureToOz ::  Procedure     --  The procedure getting transformed.
                  -> ExprTable  --  List of the types for each expression.
                  -> Int        --  The label-number.
                  -> ProcTable  --  Arguments of every procedure.
                  -> OzCode     --  Output code.
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

--  Helper function for procedureToOz. Store the registers in corresponding 
--  slots in the stack for each argument of the procedure.
argsToCode :: Int                 --  The register-number.
              -> [FormalArgSpec]  --  List of the arguments
              -> SlotStack        --  The slot-numbers for each identifier.
              -> [OzLine]         --  Output code.
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

--  Helper function for procedureToOz. Initialize all the declarations and store
--  them in corresponding slots.
initialiseDecls ::  [Decl]        --  List of declarations in the procedure.
                    -> Int        --  Gives the declaration number.
                    -> [OzLine]   --  Code to be passed and built upon.
                    -> [OzLine]   --  Output code.
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

--  Helper function for initialiseDecls. Initialise every slot corresponding to
--  the array/matrix.
initialiseShape ::  Int           --  The 'first' slot.
                    -> Int        --  The number of slots initialised.
                    -> [OzLine]   --  Code passed and built on.
                    -> [OzLine]   --  Output code.
initialiseShape start n code =
  if n == 0 
    then code
    else 
      initialiseShape start (n-1) (
        [InstrLine (Store (Slot (start + n - 1)) (Reg 0))] ++ code)

--  Given a program AST, give an Oz representation or a series of error messages
compileToOz ::  Program     --  The program getting transformed
                -> String   --  The output. OzCode or error messages.
compileToOz program =
  let 
    ParsedProg newProgram errors et = check_program program 
  in
    case errors of
      [] -> printOzCode (programToOz newProgram et)
      _ -> show errors
