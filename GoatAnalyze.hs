module GoatAnalyze where

import GoatAST
import SymTable
import GoatError
import Data.Maybe

data ParsedExpr
  = ParsedExpr Expr BaseType [ErrorMessage] ExprTable
  deriving (Show, Eq)

data ParsedStmt
  = ParsedStmt Stmt [ErrorMessage] ExprTable
  deriving (Show, Eq)

data ParsedProc = ParsedProc Procedure [ErrorMessage] ExprTable

data ParsedProg 
  = ParsedProg Program [ErrorMessage] ExprTable
  deriving (Show, Eq)

--  TODO: Give more meaningful error messages.
--  Ensure there is a "main" procedure.

--  Checks if an expression is well typed. Will return an updated expression if
--  int-to-float conversion is necessary, and a series of error messages if the
--  expression fails the type checking.

check_expr :: Expr -> LookupTable -> ParsedExpr 
check_expr expr table 
  = case expr of
      BoolCon p b
        -> ParsedExpr (BoolCon p b) BoolType [] 
          (addExprToTable p BoolType buildExprTable)
      IntCon p i
        -> ParsedExpr (IntCon p i) IntType []
          (addExprToTable p IntType buildExprTable)
      FloatCon p f
        -> ParsedExpr (FloatCon p f) FloatType []
          (addExprToTable p FloatType buildExprTable)
      StrCon p s
        -> ParsedExpr (StrCon p s) StringType []
          (addExprToTable p StringType buildExprTable)
      Id p i
        -> let lookup = getTypeFromIdent i table in
          case lookup of
            Just (_, Base g)
              -> ParsedExpr (Id p i) g [] (addExprToTable p g buildExprTable)
            _
              -> ParsedExpr (Id p i) IntType 
                [ErrorMessage p ("Undefined variable " ++ i)]
                (addExprToTable p IntType buildExprTable)        
      And p e1 e2
        -> let 
            ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
            ParsedExpr expr2 type2 message2 et2 = check_expr e2 table
            et = (addExprToTable p BoolType (exprUnion et1 et2)) 
          in
          if type1 == BoolType && type2 == BoolType 
            then 
              ParsedExpr (And p expr1 expr2) BoolType (message1 ++ message2) et
            else 
              ParsedExpr (And p expr1 expr2) BoolType (message1 ++ 
              [ErrorMessage p "Type error"] ++ message2) et
      Or p e1 e2
        -> let 
            ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
            ParsedExpr expr2 type2 message2 et2 = check_expr e2 table
            et = (addExprToTable p BoolType (exprUnion et1 et2)) 
          in
          if type1 == BoolType && type2 == BoolType 
            then 
              ParsedExpr (Or p expr1 expr2) BoolType (message1 ++ message2) et
            else 
              ParsedExpr (Or p expr1 expr2) BoolType (message1 ++ 
              [ErrorMessage p "Type error"] ++ message2) et
      Not p e1
        -> let 
            ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
            et = addExprToTable p BoolType et1
          in
          if type1 == BoolType 
            then ParsedExpr (Not p expr1) BoolType (message1) et
            else ParsedExpr (Not p expr1) BoolType (message1 ++
              [ErrorMessage p "Type error"]) et
      BinOpExp p binop e1 e2
        -> let 
            ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
            ParsedExpr expr2 type2 message2 et2 = check_expr e2 table
            expr = (BinOpExp p binop expr1 expr2)
            et = exprUnion et1 et2
          in
          case (type1, type2) of
            (IntType, IntType)
              -> 
                ParsedExpr expr IntType (message1 ++ message2) 
                (addExprToTable p IntType et)
            (FloatType, FloatType)
              -> 
                ParsedExpr expr FloatType (message1 ++ message2)
                (addExprToTable p FloatType et)
            (IntType, FloatType)
              -> ParsedExpr (BinOpExp p binop (ToFloat expr1) expr2) FloatType 
                (message1 ++ message2) (addExprToTable p FloatType et)
            (FloatType, IntType)
              -> ParsedExpr (BinOpExp p binop expr1 (ToFloat expr2)) FloatType 
                (message1 ++ message2) (addExprToTable p FloatType et)
            (_, _)
              -> ParsedExpr expr IntType 
                (message1 ++ (error_from_expr p expr) ++ message2) 
                (addExprToTable p IntType et)
      UnaryMinus p e1
        -> let 
            ParsedExpr expr1 type1 message1 et = check_expr e1 table
            expr = UnaryMinus p expr1 
          in
          case type1 of
            IntType
              -> ParsedExpr expr IntType message1 (addExprToTable p IntType et)
            FloatType
              -> 
                ParsedExpr expr FloatType message1 
                (addExprToTable p FloatType et)
            _
              -> ParsedExpr expr FloatType 
                (message1 ++ (error_from_expr p expr))
                (addExprToTable p FloatType et)
      Rel p relop e1 e2
        -> let 
            ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
            ParsedExpr expr2 type2 message2 et2 = check_expr e2 table
            et = addExprToTable p BoolType (exprUnion et1 et2) 
          in
          case (type1, type2) of
            (IntType, IntType)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++
                message2) et
            (IntType, FloatType)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++ 
                message2) et
            (FloatType, IntType)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++ 
                message2) et
            (FloatType, FloatType)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++ 
                message2) et
            (BoolType, BoolType)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++ 
                message2) et
            (_,_)
              -> ParsedExpr (Rel p relop expr1 expr2) BoolType (message1 ++ 
                [ErrorMessage p "Type error"] ++ message2) et
      ArrayRef p ident e1
        -> let 
          ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
          lookup = getTypeFromIdent ident table
          expr = (ArrayRef p ident expr1)
          err = error_from_expr p expr
          in
          case lookup of
            Just (_, Array g num)
              -> if type1 == IntType 
                then ParsedExpr expr g message1 (addExprToTable p g et1)
                else ParsedExpr expr g (message1 ++ err) 
                (addExprToTable p g et1)
            _
              -> ParsedExpr expr IntType (message1 ++ err) 
              (addExprToTable p IntType et1)
      MatrixRef p ident e1 e2
        -> let 
          ParsedExpr expr1 type1 message1 et1 = check_expr e1 table
          ParsedExpr expr2 type2 message2 et2 = check_expr e2 table
          et = exprUnion et1 et2 
          lookup = getTypeFromIdent ident table in
          case lookup of
            Just (_, Matrix g num1 num2)
              -> if type1 == IntType && type2 == IntType 
                then ParsedExpr (MatrixRef p ident expr1 expr2) g 
                  (message1 ++ message2) (addExprToTable p g et)
                else ParsedExpr (MatrixRef p ident expr1 expr2) g (message1 ++ 
                  [ErrorMessage p "Indexing with non-Int"] ++ message2) 
                  (addExprToTable p g et)
            _
              -> ParsedExpr (MatrixRef p ident expr1 expr2) IntType (message1 ++ 
                [ErrorMessage p "Variable is not a matrix"] ++ message2) 
                (addExprToTable p IntType et)

check_types_match :: [BaseType] -> [BaseType] -> Bool
check_types_match [] [] = True
check_types_match ((IntType):es) ((FloatType):as) = check_types_match es as
check_types_match (e:es) (a:as) = 
  if e == a then check_types_match es as else False

check_stmt :: Stmt -> LookupTable -> ProcTable -> ParsedStmt
check_stmt stmt table procedures 
  = case stmt of
    Assign p lvalue expr
      -> let 
          ParsedExpr new_expr expr_type message et = check_expr expr table
          good = ParsedStmt (Assign p lvalue new_expr) message et
          bad = 
            ParsedStmt (Assign p lvalue new_expr) (message ++ 
            [ErrorMessage p "Bad assignment"]) et
        in
        case lvalue of
        LId p2 ident
          -> let lookup = getTypeFromIdent ident table in
            case lookup of
              Just (_,goat_type)
                -> case goat_type of
                  Base basetype
                    -> case (basetype, expr_type) of
                        (FloatType, IntType) 
                          -> 
                            ParsedStmt (Assign p lvalue (ToFloat new_expr)) 
                            message et
                        _ -> if basetype == expr_type then good else bad
                  _ -> bad
              _ -> bad

        LArrayRef p2 ident expr1
          -> let lookup = getTypeFromIdent ident table in
            case lookup of
              Just (_,goat_type)
                -> case goat_type of
                  Array basetype n
                      -> case (basetype, expr_type) of
                        (FloatType, IntType) -> good
                        _ -> if basetype == expr_type then good else bad
                  _ -> bad
              _ -> bad

        LMatrixRef p2 ident expr1 expr2
          -> let lookup = getTypeFromIdent ident table in
            case lookup of
              Just (_,goat_type)
                -> case goat_type of
                  Matrix basetype m n
                      -> case (basetype, expr_type) of
                        (FloatType, IntType) -> good
                        _ -> if basetype == expr_type then good else bad
                  _ -> bad
              _ -> bad
    Read p lvalue
      -> let
        bad = 
          ParsedStmt (Read p lvalue) [ErrorMessage p "Type error"] 
          buildExprTable
        in
        case lvalue of
          LId p2 ident
            -> let 
              lookup = getTypeFromIdent ident table 
              good = ParsedStmt (Read p lvalue) [] buildExprTable
              in
              case lookup of
                Just (_,goat_type)
                  -> case goat_type of
                    Base _ -> good
                    _ -> bad
                _ -> bad
          LArrayRef p2 ident expr1
            -> let 
              lookup = getTypeFromIdent ident table
              ParsedExpr new_expr expr_type message et = check_expr expr1 table
              good = 
                ParsedStmt (Read p (LArrayRef p2 ident new_expr)) message et
              in
              case lookup of
                Just (_,goat_type)
                  -> case goat_type of
                    Array _ _ -> 
                      if expr_type == IntType then good else bad
                    _ -> bad
                _ -> bad
          LMatrixRef p2 ident expr1 expr2
            -> let 
              lookup = getTypeFromIdent ident table
              ParsedExpr new_expr_1 type_1 message_1 et1 = 
                check_expr expr1 table
              ParsedExpr new_expr_2 type_2 message_2 et2 = 
                check_expr expr2 table
              et = exprUnion et1 et2
              good = 
                ParsedStmt (Read p (LMatrixRef p2 ident 
                new_expr_1 new_expr_2)) (message_1 ++ message_2) et

              in
              case lookup of
                Just (_,goat_type)
                  -> case goat_type of
                    Array _ _ -> 
                      if type_1 == IntType && type_2 == IntType 
                        then good 
                        else bad
                    _ -> bad
                _ -> bad
    Write p expr
      -> let
          (ParsedExpr new_expr type_1 message et) = check_expr expr table 
        in
        ParsedStmt (Write p new_expr) message buildExprTable
    ProcCall p ident exprs
      -> let 
          parsed_exprs = map (\x -> check_expr x table) exprs
          get_expr (ParsedExpr expr basetype err et) = expr
          get_err (ParsedExpr expr basetype err et) = err
          get_type (ParsedExpr expr basetype err et) = basetype
          get_table (ParsedExpr expr basetype err et) = et
          new_exprs = map (\x -> get_expr x) parsed_exprs
          new_err = concat (map (\x -> get_err x) parsed_exprs)
          new_types = map (\x -> get_type x) parsed_exprs
          new_et = exprUnions (map (\x -> get_table x) parsed_exprs)
          exprs_length = length exprs
          proc = searchProcTable ident procedures
        in
        case proc of
          Just (Procedure p2 _ args _ _)
            -> if length args /= exprs_length 
              then ParsedStmt (ProcCall p ident new_exprs) (new_err ++ 
                [ErrorMessage p "Wrong number of arguments"]) new_et
              else 
                let 
                  type_from_arg (FormalArgSpec _ _ b _) = b
                  arg_types = map (\x -> type_from_arg x) args
                in
                  if check_types_match new_types arg_types 
                    then ParsedStmt (ProcCall p ident new_exprs) new_err new_et
                    else ParsedStmt (ProcCall p ident new_exprs) (new_err
                      ++ [ErrorMessage p "Type mismatch"]) new_et
          Nothing
            -> ParsedStmt (ProcCall p ident new_exprs) (new_err ++ 
            [ErrorMessage p "Undefined procedure"]) new_et
    If pos expr stmts
      -> let
          ParsedExpr new_expr type_1 message et = check_expr expr table
          parsed_stmts = map (\x -> check_stmt x table procedures) stmts
          get_stmt (ParsedStmt new_stmt err et1) = new_stmt
          get_err (ParsedStmt new_stmt err et1) = err
          get_table (ParsedStmt new_stmt err et1) = et1
          new_stmts = map (\x -> get_stmt x) parsed_stmts
          new_err = concat (map (\x -> get_err x) parsed_stmts)
          new_et = exprUnions (map (\x -> get_table x) parsed_stmts)
        in
          case type_1 of
            BoolType -> ParsedStmt (If pos new_expr new_stmts) new_err new_et
            _ 
              -> ParsedStmt (If pos new_expr new_stmts) (new_err ++ 
              [ErrorMessage pos "Non boolean in if statement"]) new_et
    IfElse pos expr stmts1 stmts2
      -> let
          ParsedExpr new_expr type_1 message et = check_expr expr table
          parsed_stmts_1 = map (\x -> check_stmt x table procedures) stmts1
          parsed_stmts_2 = map (\x -> check_stmt x table procedures) stmts2
          get_stmt (ParsedStmt new_stmt err et1) = new_stmt
          get_err (ParsedStmt new_stmt err et1) = err
          get_table (ParsedStmt new_stmt err et1) = et1
          new_stmts_1 = map (\x -> get_stmt x) parsed_stmts_1
          new_stmts_2 = map (\x -> get_stmt x) parsed_stmts_2
          new_err_1 = concat (map (\x -> get_err x) parsed_stmts_1)
          new_err_2 = concat (map (\x -> get_err x) parsed_stmts_2)
          new_et_1 = exprUnions (map (\x -> get_table x) parsed_stmts_1)
          new_et_2 = exprUnions (map (\x -> get_table x) parsed_stmts_2)
          new_et = exprUnion new_et_1 new_et_2
        in
          case type_1 of
            BoolType 
              -> ParsedStmt (IfElse pos new_expr new_stmts_1 new_stmts_2) 
                (new_err_1 ++ new_err_2) new_et
            _
              -> ParsedStmt (IfElse pos new_expr new_stmts_1 new_stmts_2) 
                (new_err_1 ++ new_err_2 ++ 
                [ErrorMessage pos "Non boolean in if statement"]) new_et
    While pos expr stmts
      -> let
          ParsedExpr new_expr type_1 message et = check_expr expr table
          parsed_stmts = map (\x -> check_stmt x table procedures) stmts
          get_stmt (ParsedStmt new_stmt err et1) = new_stmt
          get_err (ParsedStmt new_stmt err et1) = err
          get_table (ParsedStmt new_stmt err et1) = et1
          new_stmts = map (\x -> get_stmt x) parsed_stmts
          new_err = concat (map (\x -> get_err x) parsed_stmts)
          new_et = exprUnions (map (\x -> get_table x) parsed_stmts)
        in
          case type_1 of
            BoolType -> ParsedStmt (While pos new_expr new_stmts) new_err new_et
            _ 
              -> ParsedStmt (While pos new_expr new_stmts) (new_err ++ 
              [ErrorMessage pos "Non boolean in if statement"]) new_et

check_procedure :: Procedure -> ProcTable -> ParsedProc
check_procedure (Procedure pos ident args decls stmts) proc_list = 
  let 
    table = buildLookupTable (Procedure pos ident args decls stmts)
    parsed_stmts = map (\x -> check_stmt x table proc_list) stmts
    get_stmt (ParsedStmt new_stmt err et) = new_stmt
    get_err (ParsedStmt new_stmt err et) = err
    get_table (ParsedStmt new_stmt err et) = et
    new_stmts = map (\x -> get_stmt x) parsed_stmts
    new_err = concat (map (\x -> get_err x) parsed_stmts)
    new_et = exprUnions (map (\x -> get_table x) parsed_stmts)
  in
    ParsedProc (Procedure pos ident args decls new_stmts) new_err new_et

check_program :: Program -> ParsedProg
check_program (Program procedures) =
  let
    proctable = buildProcTable procedures
    parsed_procedures = map (\x -> check_procedure x proctable) procedures
    get_proc (ParsedProc proc error et) = proc
    get_err (ParsedProc proc error et) = error
    get_table (ParsedProc proc error et) = et
    new_procs = map (\x -> get_proc x) parsed_procedures
    new_err = concat (map (\x -> get_err x) parsed_procedures)
    new_et = exprUnions (map (\x -> get_table x) parsed_procedures)
  in
    ParsedProg (Program new_procs) new_err new_et