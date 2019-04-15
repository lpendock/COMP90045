module Main where

import GoatAST
import Data.Char
import Data.Either
import Data.Maybe
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

--  This is essentially a modified version of KidParser.hs

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
--  header "proc", followed by the function name and the program body.
-----------------------------------------------------------------

pProg :: Parser GoatProgram
pProg
    = do
        reserved "proc"
        name <- identifier
        header <- pHeader
        (decls,stmts) <- pProgBody
        return (Program name header decls stmts)

--  Parse the program header, containing comma-separated parameters within a set 
--  of parenthesis

pHeader :: Parser [Parameter]
pHeader
    = do
        reservedOp "("
        parameters <- pParameter `sepBy` comma
        reservedOp ")"
        return parameters

--  Parse a parameter, containing the keyword "ref" or "val" followed by their
--  basetype and identifier

pParameter :: Parser Parameter
pParameter
    = do    { reserved "ref"; baseType <- pBaseType; ident <- identifier
            ; return (Ref baseType ident) 
            } 
    <|>
    do  { reserved "val"; baseType <- pBaseType; ident <- identifier
        ; return (Val baseType ident) 
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

--  Return an If or IfElse statement depending on if the parser detects the 
--  keyword fi or else.

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
--  pExp is the main parser for expressions.
-----------------------------------------------------------------

pExp, pFactor, pNum, pIdent, pString, pBool :: Parser Expr

pExp = buildExpressionParser table pFactor
    <?>
    "expression"

-- The table enforces the order of precedence.

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

--  Checks if an identifier is followed by a representation of an 
--  address/offset/list/goat-pun

pAddr :: Parser Address
pAddr 
    = do    {
            char '['; m <- natural; addr <- choice[pArray m, pMatrix m]
            ; return addr
            }
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

data Task 
    = Compile
    | Pprint
    | Parse
    deriving (Show, Eq)

main :: IO ()
main
  = do
      progname <- getProgName
      args <- getArgs
      task <- checkArgs progname args
      if task == Compile then
        do
          putStrLn "Sorry, cannot generate code yet"
          exitWith ExitSuccess
      else
        if task == Parse then
          do
            let [_, filename] = args
            input <- readFile filename
            let output = runParser pMain 0 "" input
            case output of
              Right tree -> putStrLn (show tree)
              Left err -> do { putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }
        else
          do
            let [_, filename] = args
            input <- readFile filename
            let output = runParser pMain 0 "" input
            case output of
              Right tree -> putStrLn (prettyProgram tree)
              Left err -> do { putStr "Parse error at "
                             ; print err
                             ; exitWith (ExitFailure 2)
                             }

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_]
  = do
      putStrLn ("Missing filename")
      exitWith (ExitFailure 1)
checkArgs _ [filename]
  = return Compile
checkArgs _ ["-p", filename]
  = return Pprint
checkArgs _ ["-a", filename]
  = return Parse
checkArgs progname _
  = do
      putStrLn ("Usage: " ++ progname ++ " [-p] filename")
      exitWith (ExitFailure 1)

