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

data Binop
    = Op_add | Op_mul | Op_sub | Op_div | Op_or | Op_and | Op_equal 
    | Op_not_equal | Op_less | Op_lequal | Op_greater | Op_grequal
    deriving (Show, Eq)

data Expr
    = BoolConst Bool
    | IntConst Int
    | StrConst String
    | FloatConst Double
    | Id Ident Address
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
    deriving (Show, Eq)

data Decl
    = Decl Ident BaseType Address
    deriving (Show, Eq)

data Address 
    = Array Integer 
    | Matrix Integer Integer
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