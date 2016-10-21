module Main where

import System.IO
import System.Environment
import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

{- Defining ADT for contracts -}
type Type = String

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
ruleExample = "rule_of_withdrawal (ac: ACCOUNT; n, initial_amount: INTEGER;): \
       \ {ac.amount = initial_amount; ac.amount <= n} ac.withdraw (n) {ac.amount = initial_amount - n}"

{- Defining parsers -}

symbol :: Parser Char
symbol = oneOf "-_!?"

around :: Parser a -> Parser String
around p = many space >> p >> many space

semiColon = char ';'
colon = char ':'

semiOrParenR :: Parser Char
semiOrParenR = oneOf ";)"


parseString :: Parser String
parseString = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let s = first:rest
  return s

parseVariable :: Parser Variable -- TODO parse this: "a,b:TYPE"
parseVariable = do
  varName <- parseString
  around colon
  varType <- parseString
  return $ Variable (varName) (varType)

parseVariables :: Parser Variables
parseVariables = do
  char '('
  vars <- sepBy parseVariable (around semiColon)
  char ')'
  return vars

--parseRule :: Parser Rule
--parseRule =


readExpr :: String -> String
readExpr input = case parse parseVariables "hybrid" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
