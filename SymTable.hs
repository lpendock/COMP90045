module SymTable where

import GoatAST
import Data.Map.Strict as Map
import Data.Maybe
import OzTree

type LookupTable = Map Ident (ParMode, GoatType)
type ProcTable = Map Ident Procedure

buildEmptyTable :: LookupTable
buildEmptyTable = Map.empty

buildLookupTable :: Procedure -> LookupTable
buildLookupTable (Procedure _ _ argspec decl _) =
  let
    from_arg_spec (FormalArgSpec _ parmode basetype ident)
      = (ident, (parmode, (Base basetype)))
    from_decl (Decl _ ident goattype) = (ident, (Val, goattype))
  in
    Map.fromList ((Prelude.map from_arg_spec argspec) 
    ++ (Prelude.map from_decl decl))

getTypeFromIdent :: Ident -> LookupTable -> Maybe (ParMode, GoatType)
getTypeFromIdent ident table = Map.lookup ident table

getTypeFromLvalue :: Lvalue -> LookupTable -> Maybe (ParMode, GoatType)
getTypeFromLvalue lvalue table =
  case lvalue of
    LId _ ident ->  Map.lookup ident table
    LArrayRef _ ident _ ->  Map.lookup ident table
    LMatrixRef _ ident _ _ ->  Map.lookup ident table

buildProcTable :: [Procedure] -> ProcTable
buildProcTable procedures =
  let
    ident (Procedure _ i _ _ _) = i
  in
    Map.fromList (Prelude.map (\x -> (ident x, x)) procedures)

searchProcTable :: Ident -> ProcTable -> Maybe Procedure
searchProcTable ident table = Map.lookup ident table

type ExprTable = Map Pos BaseType

buildExprTable :: ExprTable
buildExprTable = Map.empty

addExprToTable :: Pos -> BaseType -> ExprTable -> ExprTable
addExprToTable pos basetype table = insert pos basetype table

typeFromPos :: Pos -> ExprTable -> Maybe BaseType
typeFromPos pos table = Map.lookup pos table

typeFromExpr :: Expr -> ExprTable -> Maybe BaseType
typeFromExpr expr table =
  case expr of
    BoolCon p _ -> Just BoolType
    And p _ _ -> typeFromPos p table
    Or p _ _ -> typeFromPos p table
    Not p _ -> typeFromPos p table
    Rel p _ _ _ -> typeFromPos p table
    IntCon p _ -> Just IntType
    FloatCon p _ -> Just FloatType
    ToFloat _ -> Just FloatType
    StrCon p _ -> Just StringType
    Id p _ -> typeFromPos p table
    ArrayRef p _ _ -> typeFromPos p table
    MatrixRef p _ _ _ -> typeFromPos p table
    BinOpExp p _ _ _ -> typeFromPos p table
    UnaryMinus p _ -> typeFromPos p table

exprUnion :: ExprTable -> ExprTable -> ExprTable
exprUnion x y = Map.union x y

exprUnions :: [ExprTable] -> ExprTable
exprUnions xs = Map.unions xs

data SlotStack 
  = SlotStack Int (Map Ident Slot)

buildStack :: SlotStack
buildStack = SlotStack 0 Map.empty

addIdentToStack :: Ident -> SlotStack -> SlotStack
addIdentToStack ident (SlotStack size slots) = 
  SlotStack (size+1) (insert ident (Slot size) slots)

addIdentsToStack :: [Ident] -> SlotStack -> SlotStack
addIdentsToStack idents stack = 
  case idents of
    [] -> stack
    (x:xs) -> addIdentsToStack xs (addIdentToStack x stack)

deleteIdentFromStack :: Ident -> SlotStack -> SlotStack
deleteIdentFromStack ident (SlotStack size stack) =
  SlotStack (size-1) (delete ident stack)

deleteIdentsFromStack :: [Ident] -> SlotStack -> SlotStack
deleteIdentsFromStack idents stack =
  case idents of
    [] -> stack
    (i:is) -> deleteIdentsFromStack is (deleteIdentFromStack i stack)

slotFromIdent :: Ident -> SlotStack -> Maybe Slot
slotFromIdent ident (SlotStack size stack) = Map.lookup ident stack

slotFromLvalue :: Lvalue -> SlotStack -> Maybe Slot
slotFromLvalue lvalue (SlotStack size stack) =
  case lvalue of
    LId _ ident ->  Map.lookup ident stack
    LArrayRef _ ident _ ->  Map.lookup ident stack
    LMatrixRef _ ident _ _ ->  Map.lookup ident stack