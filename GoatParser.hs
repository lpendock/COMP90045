module Main where

import GoatAST
import Data.Char
import Data.Either
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit
import GoatPrinter

import Data.Typeable

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
naturalOrFloat = Q.naturalOrFloat lexer
float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
dot        = Q.dot lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
brackets   = Q.brackets lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved = 
    ["begin", "bool", "call", "do", "else", "end", "false", "fi", "float", "if", 
        "int", "od", "proc", "read", "ref", "then", "true", "val", "while", 
        "write"]

myOpnames 
    = ["||", "&&", "!", "=", "!=", "<", "<=", ">", ">=", "+", "-", "*", "/", 
    ":=", "(", ")", "[", "]"]

-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
    = do
        reserved "proc"
        name <- identifier
        header <- pHeader
        (decls,stmts) <- pProgBody
        return (Program name header decls stmts)

pHeader :: Parser [Parameter]
pHeader
    = do
        reservedOp "("
        parameters <- pParameter `sepBy` comma
        reservedOp ")"
        return parameters

pParameter :: Parser Parameter
pParameter
    = do { reserved "ref"; baseType <- pBaseType; ident <- identifier; return (Ref baseType ident) 
        } 
    <|>
    do { reserved "val"; baseType <- pBaseType; ident <- identifier; return (Val baseType ident) 
        } 

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
        addr <- pAddr
        whiteSpace
        semi
        return (Decl ident basetype addr)

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

pStmt, pRead, pWrite, pAsg, pCall, pIf, pWhile :: Parser Stmt

pStmt 
    = choice [pRead, pWrite, pAsg, pCall, pIf, pWhile] 

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

pIf
    = do
        reserved "if"
        lvalue <- pExp
        reserved "then"
        rvalue <- many1 pStmt
        stmt <- choice [pJustIf lvalue rvalue, pIfElse lvalue rvalue]
        return stmt

pJustIf :: Expr -> [Stmt] -> Parser Stmt
pJustIf lvalue rvalue
    = do
        reserved "fi"
        return (If lvalue rvalue)

pIfElse :: Expr -> [Stmt] -> Parser Stmt
pIfElse lvalue mvalue
    = do
        reserved "else"
        rvalue <- many1 pStmt
        reserved "fi"
        return (IfElse lvalue mvalue rvalue)

pWhile
    = do
        reserved "while"
        lvalue <- pExp
        reserved "do"
        rvalue <- many1 pStmt
        reserved "od"
        return (While lvalue rvalue)

-----------------------------------------------------------------
--  pExp is the main parser for expressions. It takes into account
--  the operator precedences and the fact that the binary operators
--  are left-associative.
-----------------------------------------------------------------

pExp, pFactor, pNum, pIdent, pString, pBool :: Parser Expr

pExp = buildExpressionParser table pFactor
    <?>
    "expression"

table = [ [ prefix "-" UnaryMinus ]
        , [ binary "*" (Binop Mul), binary "/" (Binop Div) ]
        , [ binary "+" (Binop Add), binary "-" (Binop Sub) ]
        , [ relation "=" (Binop Equal), relation "!=" (Binop NotEqual)
            , relation ">" (Binop Greater), relation ">=" (Binop Grequal)
            , relation "<" (Binop Less), relation "<=" (Binop Lequal)
          ]
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

pBool
    = do
        reserved "true"
        return (BoolConst True)  
    <|>
    do
        reserved "false"
        return (BoolConst False) 

pFactor
  = choice [parens pExp, pNum, pIdent, pBool]
    <?> 
    "\"factor\""

pNum
  = do
      n <- naturalOrFloat <?> ""
      if isLeft n then 
        return (IntConst (fromInteger (fromLeft 0 n) :: Int))
      else 
        return (FloatConst (fromRight 0 n))
    <?>
    "number"

pIdent 
  = do
      ident <- identifier
      addr <- pAddr
      whiteSpace
      return (Id ident addr)
    <?>
    "identifier"

pLvalue :: Parser Lvalue
pLvalue
  = do
      ident <- identifier
      addr <- pAddr
      whiteSpace
      return (LId ident addr)
    <?>
    "lvalue"

pAddr :: Parser Address
pAddr 
    = do {char '['; m <- natural; addr <- choice[pArray m, pMatrix m]; return addr }
    <|>
    do { return NoAddress }
    <?>
    "address"

pArray, pMatrix :: Integer -> Parser Address

pArray n
    = do
        char ']'
        return (Array n)

pMatrix m
    = do
        comma
        n <- natural
        char ']'
        return (Matrix m n)
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser [GoatProgram]
pMain
  = do
      whiteSpace
      p <- many pProg
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
           Right ast -> putStr (prettyProgram ast)
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
