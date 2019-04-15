module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data BaseType
    = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue
    = LId Ident Address
    deriving (Show, Eq)

data Operator
    = Add 
    | Mul 
    | Sub 
    | Div 
    | Or 
    | And 
    | Equal 
    | NotEqual 
    | Less 
    | Lequal 
    | Greater 
    | Grequal
    deriving (Show, Eq)

data Expr
    = BoolConst Bool
    | IntConst Int
    | StrConst String
    | FloatConst Double
    | Id Ident Address
    | Not Expr
    | Binop Operator Expr Expr
    | UnaryMinus Expr
    deriving (Show, Eq)

data Decl
    = Decl Ident BaseType Address
    deriving (Show, Eq)

data Address 
    = Array Expr 
    | Matrix Expr Expr
    | NoAddress
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

data Parameter
    = Ref BaseType Ident
    | Val BaseType Ident
    deriving (Show, Eq)

data GoatProgram
    = Program String [Parameter] [Decl] [Stmt]
    deriving (Show, Eq)