module GoatPrinter where

import GoatAST
import Data.List

prettyProgram :: [GoatProgram] -> String
prettyProgram x = intercalate "\n" (map prettyFunction x)

prettyFunction :: GoatProgram -> String
prettyFunction (Program name params decls stmts)
    = "proc " ++ name ++ " (" ++ (prettyParameters params) ++ ")" ++ 
    (prettyDeclarations decls) ++  "begin\n" ++ (prettyStatements stmts 1) ++ 
    "\nend\n"

prettyParameters :: [Parameter] -> String
prettyParameters x = intercalate ", " (map prettyParameter x)

prettyParameter :: Parameter -> String
prettyParameter (Ref basetype ident)
    = "ref " ++ (prettyBaseType basetype) ++ " " ++ ident
prettyParameter (Val basetype ident)
    = "val " ++ (prettyBaseType basetype) ++ " " ++ ident

prettyBaseType :: BaseType -> String
prettyBaseType BoolType = "bool"
prettyBaseType IntType = "int"
prettyBaseType FloatType = "float"

prettyDeclarations :: [Decl] -> String
prettyDeclarations [] = "\n"
prettyDeclarations x = 
    "\n\t" ++ intercalate "\n\t" (map prettyDeclaration x) ++ "\n"

prettyDeclaration :: Decl -> String
prettyDeclaration (Decl ident basetype addr) =
    (prettyBaseType basetype) ++ " " ++ ident ++ (prettyAddress addr) ++ ";"

prettyAddress :: Address -> String
prettyAddress NoAddress = ""
prettyAddress (Array n) = "[" ++ (show n) ++ "]"
prettyAddress (Matrix m n) = "[" ++ (show m) ++ "," ++ (show n) ++ "]"

prettyStatements :: [Stmt] -> Int -> String
prettyStatements x n = 
    indent n ++ (intercalate ("\n" ++ (indent n)) 
    (map (\y -> prettyStatement y n) x))

indent :: Int -> String
indent n = concat (replicate n "\t")

prettyLvalue :: Lvalue -> String
prettyLvalue (LId ident addr) = ident ++ (prettyAddress addr)

prettyStatement :: Stmt -> Int -> String
prettyStatement (Assign lvalue expr) n = 
    prettyLvalue lvalue ++ " := " ++ (prettyExpression expr) ++ ";"
prettyStatement (Read lvalue) _ =
     "read " ++ (prettyLvalue lvalue) ++ ";"
prettyStatement (Write expr) _ = 
    "write " ++ (prettyExpression expr) ++ ";"
prettyStatement (Call lvalue expr) _ =
    "call " ++ (prettyLvalue lvalue) ++ "(" ++ (prettyExpressions expr) ++ ")" 
    ++ ";"
prettyStatement (If expr stmts) n =
    "if " ++ (prettyExpression expr) ++ " then\n" ++ 
    (prettyStatements stmts (n+1)) ++ "\n" ++ (indent n) ++ "fi"
prettyStatement (IfElse expr succ fail) n = 
    "if " ++ (prettyExpression expr) ++ "\n" ++ (prettyStatements succ (n+1)) ++
    "\n" ++ (indent n) ++ "else " ++ "\n" ++ (prettyStatements fail (n+1)) ++ 
    "\n" ++ (indent n) ++ "fi"
prettyStatement (While expr stmts) n =
    "while " ++ (prettyExpression expr) ++ " do\n" ++ 
    (prettyStatements stmts (n+1)) ++ "\n" ++ (indent n) ++ "od"

prettyExpressions :: [Expr] -> String
prettyExpressions x = intercalate "\n" (map prettyExpression x)

removeDoubleSlash :: String -> String
removeDoubleSlash s = read $ "\"" ++ s ++ "\""

prettyExpression :: Expr -> String
prettyExpression (BoolConst False) = "false"
prettyExpression (BoolConst True) = "true"
prettyExpression (IntConst n) = show n
prettyExpression (StrConst s) = show (removeDoubleSlash s)
prettyExpression (FloatConst f) = show f
prettyExpression (Id ident addr) = ident ++ (prettyAddress addr)
prettyExpression (Binop op a b) = 
    "" ++ prettyExpression' a ++ " " ++ (prettyOperator op) ++ " " 
    ++ (prettyExpression' b) ++ ""
prettyExpression (Not a) = "!" ++ (prettyExpression' a)
prettyExpression (UnaryMinus a) = "-" ++ (prettyExpression' a)

prettyExpression' :: Expr -> String
prettyExpression' (Binop op a b) = "(" ++ prettyExpression (Binop op a b) ++ ")"
prettyExpression' (Not a) = "!" ++ (prettyExpression' a)
prettyExpression' (UnaryMinus a) = "-" ++ (prettyExpression' a)
prettyExpression' x = prettyExpression x

prettyOperator :: Operator -> String
prettyOperator Add = "+"
prettyOperator Sub = "-"
prettyOperator Div = "/"
prettyOperator Mul = "*"
prettyOperator Equal = "="
prettyOperator NotEqual = "!="
prettyOperator And = "&&"
prettyOperator Or = "||"
prettyOperator Greater = ">"
prettyOperator Grequal = ">="
prettyOperator Less = "<="
prettyOperator Lequal = "<"