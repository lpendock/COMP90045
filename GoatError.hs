module GoatError where

import GoatAST
import PrettyPrinter
import Data.List

data ErrorMessage
  = ErrorMessage Pos String
  deriving (Eq)

instance Show ErrorMessage where
  show (ErrorMessage (x,y) message) = 
    (show x) ++ ":" ++ (show y) ++ ": error:\n" ++ message ++ "\n"

instance Ord ErrorMessage where
  (ErrorMessage pos1 _) `compare` (ErrorMessage pos2 _) = pos1 `compare` pos2

print_errors :: [ErrorMessage] -> String
print_errors errors = concat (map show (sort errors))

error_from_expr :: Pos -> Expr -> [ErrorMessage]
error_from_expr pos expr = 
  [ErrorMessage pos ("In the expression:\n" ++ (ppExp False expr))]

posFromExpr :: Expr -> Pos
posFromExpr expr =
  case expr of
    BoolCon pos _ 
      -> pos 
    And pos _ _
      -> pos
    Or pos _ _
      -> pos
    Not pos _
      -> pos
    Rel pos _ _ _
      -> pos
    IntCon pos _
      -> pos
    FloatCon pos _
      -> pos
    ToFloat _
      -> (0,0)
    StrCon pos _
      -> pos
    Id pos _
      -> pos
    ArrayRef pos _ _
      -> pos
    MatrixRef pos _ _ _
      -> pos
    BinOpExp pos _ _ _
      -> pos
    UnaryMinus pos _
      -> pos