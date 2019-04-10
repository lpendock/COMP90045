module Main where

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
	= Parsec String Int a

lexer :: Q.TokenParser Int
lexer
	= Q.makeTokenParser
		(emptyDef
		{ Q.commentLine     = "#"
		, Q.nestedComments  = False
		, Q.identStart      = letter
		, Q.identLetter     = alphaNum <|> char '_'
		, Q.opStart         = oneOf "|&!=<>+-*/:"
		, Q.reservedNames   = myReserved
		, Q.reservedOpNames = myOpnames
		})

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
natural    = Q.natural lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved = 
	["begin", "bool", "call", "do", "else", "end", "false", "fi", "float", "if", "int", "od", "proc", "read", "ref", "then", "true", "val", "while", "write"]

myOpnames 
	= ["||", "&&", "!", "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", ":=", "(", ")"]

-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
	= do
		reserved "proc"
		reserved "main"
		parens (return ())
		(decls,stmts) <- pProgBody
		return (Program decls stmts)

-----------------------------------------------------------------
--  pProgBody looks for a sequence of declarations followed by a
--  sequence of statements.
-----------------------------------------------------------------

pProgBody :: Parser ([Decl],[Stmt])
pProgBody
	= do
		decls <- many pDecl
		reserved "begin"
		stmts <- many1 pStmt
		reserved "end"
		return (decls,stmts)

pDecl :: Parser Decl
pDecl
	= do
		basetype <- pBaseType
		ident <- identifier
		whiteSpace
		semi
		return (Decl ident basetype)

pBaseType :: Parser BaseType
pBaseType
	= do { reserved "bool"; return BoolType }
		<|>
		do { reserved "int"; return IntType }
		<|>
		do { reserved "float"; return FloatType}

-----------------------------------------------------------------
--  pStmt is the main parser for statements. It wants to recognise
--  read and write statements, and assignments.
-----------------------------------------------------------------

pStmt, pRead, pWrite, pAsg, pCall :: Parser Stmt

pStmt 
	= choice [pRead, pWrite, pAsg, pCall]

pRead
	= do 
		reserved "read"
		lvalue <- pLvalue
		semi
		return (Read lvalue)

pWrite
	= do 
		reserved "write"
		exp <- (pString <|> pExp)
		semi
		return (Write exp)

pAsg
	= do
		lvalue <- pLvalue
		reservedOp ":="
		rvalue <- pExp
		semi
		return (Assign lvalue rvalue)

pCall
	= do 
		reserved "call"
		lvalue <- pLvalue
		reservedOp "("
		rvalue <- many (pExp)
		reservedOp ")"
		semi
		return (Call lvalue rvalue)

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------

pExp, pFactor, pUminus, pNum, pIdent, pString :: Parser Expr



pAddOp, pMulOp, pSubOp, pDivOp :: Parser (Expr -> Expr -> Expr)
pEqOp, pNeqOp, pLeqOp, pGeqOp :: Parser (Expr -> Expr -> Expr)
pGreOp, pLesOp, pAndOp, pOrOp :: Parser (Expr -> Expr -> Expr)

pAddOp
  	= do 
		reservedOp "+"
		return Add

pMulOp
  	= do 
		reservedOp "*"
		return Mul
		  
pDivOp
	= do
		reservedOp "/"
		return Div

pSubOp
	= do
		reservedOp "-"
		return Sub

pEqOp
	= do
		reservedOp "="
		return Eq

pNeqOp
	= do
		reservedOp "!="
		return Neq

pLeqOp
	= do
		reservedOp "<="
		return Leq

pGeqOp
	= do
		reservedOp ">="
		return Geq

pGreOp
	= do
		reservedOp ">"
		return Greater

pLesOp
	= do
		reservedOp "<"
		return Less

pAndOp
	= do
		reservedOp "&&"
		return And

pOrOp
	= do
		reservedOp "||"
		return Or

pExp = buildExpressionParser table pFactor
	<?>
	"expression"

table = [ [ prefix "-" UnaryMinus ]
		, [ binary "*" Mul, binary "/" Div ]
		, [ binary "+" Add, binary "-" Sub ]
		, [ relation "=" Eq, relation "!=" Neq, relation ">" Greater, relation ">=" Geq, relation "<" Less, relation "<=" Leq]
		]

prefix name fun
		= Prefix (do { reservedOp name; return fun })

binary name op 
		= Infix (do { reservedOp name; return op }) AssocLeft

relation name rel
		= Infix (do { reservedOp name; return rel }) AssocNone

pString 
	= do
		char '"'
		str <- many (satisfy (/= '"'))
		char '"'
		return (StrConst str)
		<?>
		"string"


pFactor
  = choice [parens pExp, pNum, pIdent]
	<?> 
	"\"factor\""

pUminus
  = do 
	  reservedOp "-"
	  exp <- pFactor
	  return (UnaryMinus exp)

pNum
  = do
	  n <- natural <?> ""
	  return (IntConst (fromInteger n :: Int))
	<?>
	"number"

pIdent 
  = do
	  ident <- identifier
	  return (Id ident)
	<?>
	"identifier"

pLvalue :: Parser Lvalue
pLvalue
  = do
	  ident <- identifier
	  return (LId ident)
	<?>
	"lvalue"

-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser GoatProgram
pMain
  = do
	  whiteSpace
	  p <- pProg
	  eof
	  return p

main :: IO ()
main
  = do { progname <- getProgName
	   ; args <- getArgs
	   ; checkArgs progname args
	   ; input <- readFile (head args)
	   ; let output = runParser pMain 0 "" input
	   ; case output of
		   Right ast -> print ast
		   Left  err -> do { putStr "Parse error at "
						   ; print err
						   }
	   }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
   = return ()
checkArgs progname _
   = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
	   ; exitWith (ExitFailure 1)
	   }
