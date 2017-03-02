module Parser where

import Util ((<<))

import Control.Monad
import Text.ParserCombinators.Parsec

{- Defining ADT for contracts -}
data Variable = Variable String String
                deriving (Show)

data MyOrd = LessEq | GreatEq | Less | Great | Eq
             deriving (Show)

data Expr = EVar String | EBinary String String String | ECall String [String]
            deriving (Show)

data Statement = Assign String Expr
               | Compare String MyOrd String
               | Call String [String]
                 deriving (Show)

type RuleName = String
type Variables = [Variable]
type Preconds = [Statement]
type Body = [Statement]
type Postconds = [Statement]

data Rule = Rule RuleName Variables Preconds Body Postconds
            deriving (Show)

{- Defining parsers -}

symbol :: Parser Char
symbol = oneOf ".-_!?"

around :: Parser a -> Parser a
around p = many space >> p << many space

semiColon :: Parser Char
semiColon = char ';'

colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

varString :: Parser String
varString = do
  first <- letter <|> symbol -- first variable's symbol should not be a digit
  rest <- many (letter <|> digit <|> symbol)
  let s = first:rest
  return s

variable :: Parser Variables
variable = do
  varNames <- sepBy (around varString) (comma)
  around colon
  varType <- varString
  return $ [Variable varName varType | varName <- varNames]

variables :: Parser Variables
variables = do
  char '('
  vars <- (sepBy variable (around semiColon))
  char ')'
  return $ concat vars

eVar :: Parser Expr
eVar = do
  var <- varString
  return $ EVar var

exprString :: Parser String
exprString = many $ letter <|> digit <|> symbol

operator :: Parser String
operator =  string "+"
        <|> string "-"
        <|> string "*"
        <|> string "/"
        <|> string "^"
        <|> string "**"
        <|> string "div"
        <|> string "mod"

eBinary :: Parser Expr
eBinary = do
  var1 <- exprString
  many space
  op <- operator
  many space
  var2 <- exprString
  return $ EBinary var1 op var2

eCall :: Parser Expr
eCall = do
  var <- varString
  many space
  char '('
  vars <- sepBy (around varString) (comma)
  char ')'
  return $ ECall var vars

expression :: Parser Expr
expression = try eCall <|> eVar <|> eBinary

assignment :: Parser Statement
assignment = do
  var <- varString
  around $ char '='
  expr <- expression
  return $ Assign var expr

order :: Parser String
order =  string "<"
        <|> string "<="
        <|> string "=="
        <|> string ">="
        <|> string ">"

stringToOrder :: String -> Maybe MyOrd
stringToOrder s
              | s == "<"  = Just Less
              | s == "<=" = Just LessEq
              | s == "==" = Just Eq
              | s == ">=" = Just GreatEq
              | s == ">"  = Just Great
              | otherwise = Nothing

comparator :: Parser Statement
comparator = do
  var1 <- exprString
  many space
  ord  <- order
  many space
  var2 <- exprString
  return $ Compare var1 (stringToOrder ord) var2
--
-- call :: Parser Statement
-- call = do


statement :: Parser Statement
statement = buildExpressionParser statementGrammar term


orderTable :: OperatorTable Char () Statement
orderTable = [[Infix (binaryInfixOp "==" Eq) AssocLeft,
              --  Infix (binaryInfixOp "!=" NotEquals) AssocLeft,
               Infix (binaryInfixOp ">" Great) AssocLeft,
               Infix (binaryInfixOp ">=" GreatEq) AssocLeft,
               Infix (binaryInfixOp "<" Less) AssocLeft,
               Infix (binaryInfixOp "<=" LessEq) AssocLeft]]

binaryInfixOp :: String -> MyOrd -> Parser Statement
binaryInfixOp name op = do
  string name
  skip
  return $ \l r -> Compare l op r

infixExpr = buildExpressionParser orderTable 


readExpr :: String -> String
readExpr input = case parse statement "hybrid" input of
    Left err -> "No match: " ++ show err
    Right val -> show val
