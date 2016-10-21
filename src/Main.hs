module Main where

import System.IO
import System.Environment
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

ruleExample :: String
ruleExample = "rule_of_withdrawal (ac: ACCOUNT; n, initial_amount: INTEGER): \
       \ {ac.amount = initial_amount; ac.amount <= n} ac.withdraw (n) {ac.amount = initial_amount - n}"

{- Defining parsers -}

symbol :: Parser Char
symbol = oneOf ".-_!?"

(<<) :: Parser a -> Parser b -> Parser a
p << q = do
  x <- p
  q
  return x

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


--rule :: Parser Rule
--rule = do


readExpr :: String -> String
readExpr input = case parse variables "hybrid" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
