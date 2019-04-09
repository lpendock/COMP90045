module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data BaseType
    = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue
    = LId Ident
    deriving (Show, Eq)

data Binop
    = Op_add | Op_mul | Op_sub | Op_div | Op_or | Op_and | Op_equal | Op_not_equal 
    | Op_less | Op_lequal | Op_greater | Op_grequal
    deriving (Show, Eq)

data Expr
    = BoolConst Bool
    | IntConst Int
    | StrConst String
    | Id Ident
    | Or Expr Expr
    | And Expr Expr
    | Not Expr
    | Eq Expr Expr
    | Neq Expr Expr
    | Greater Expr Expr
    | Geq Expr Expr
    | Less Expr Expr
    | Leq Expr Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | UnaryMinus Expr
    | Paren Expr
    deriving (Show, Eq)

data Decl
    = Decl Ident BaseType
    deriving (Show, Eq)

data Stmt
    = Assign Lvalue Expr
    | Read Lvalue
    | Write Expr
    | Call Lvalue [Expr]
    | If Expr [Stmt]
    | IfElse Expr [Stmt] [Stmt]
    | While Expr [Stmt]
    deriving (Show, Eq)

data GoatProgram
    = Program [Decl] [Stmt]
    deriving (Show, Eq)