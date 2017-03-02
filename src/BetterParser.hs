module BetterParser
    (
    ) where

import Util ((<<))

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


data Expression = Expr Value
                | None

data Value = AssignVal String Expression
           | InfixVal InfixOp Expression Expression
           | CallVal String [Expression]
           | VarVal String
           | BlockVal [Expression]
           | RuleVal String [Var] Expression Expression Expression

           | StrVal String
           | CharVal Char
           deriving Show

type Type = String

data Var = Var String Type
        deriving Show

data InfixOp = Equals | Greater | Less | LessEq | GreaterEq
            deriving Show


instance Show Expression where
  show (Expr val) = "(" ++ show val ++ ")"
  show None = "None"

-- Top-level parser
parseSimple :: String -> String -> Either ParseError [Expression]
parseSimple input filename = parse expressions filename input

expressions :: Parser [Expression]
expressions = do
  exprs <- many $ try expression
  skipped eof
  return exprs

expression :: Parser Expression
expression = do
  try infixExpr <|> nonInfixExpr

nonInfixExpr :: Parser Expression
nonInfixExpr = do
  let parsers = [ruleExpr, charExpr, stringExpr, assignExpr, callExpr, varExpr]--parens expression, assignExpr, callExpr, varExpr]
      attempts = map try parsers
  out <- foldl (<|>) (head parsers) (tail parsers)
  skip
  return out

ruleExample = "rule withdrawal (ac: ACCOUNT; initial_amount: INTEGER) \
              \ {amount = initial_amount; amount <= n} {ac.withdraw (n)} {ac.amount = initial_amount - n}"

ruleExpr :: Parser Expression
ruleExpr = do
  skip
  string "rule"
  ruleName <- identifier
  -- many space
  args <- parens varList
  preconds <- block
  body <- block
  postconds <- block
  return $ Expr $ RuleVal ruleName args preconds body postconds

numExpr :: Parser Expression
numExpr = do
  skip
  str <- many1 (oneOf "0123456789")
  return $ Expr pos $ NumVal $ fst (readDec str !! 0)
 
charExpr :: Parser Expression
charExpr = do
  skip
  char '\''
  c <- anyChar
  if c == '\\'
  then do
    myChar <- anyChar
    char '\''
    return $ Expr $ CharVal $ read ("'" ++ [myChar] ++ "'")
  else do
    char '\''
    return $ Expr $ CharVal c

-- Convert a string literal into an expression
stringExpr :: Parser Expression
stringExpr = do
  skip
  str <- stringLiteral
  return $ Expr $ StrVal str

-- Parse a string literal
stringLiteral :: Parser String
stringLiteral = try multiLineString <|> singleLineString

-- Allow multiple-line string literals with <<< and >>>
multiLineString :: Parser String
multiLineString = do
  skipped $ string "<<<"

  str <- many $ (notFollowedBy (string ">>>")) >> anyChar
  skipped $ string ">>>"
  return str

-- Allow single-line strings with quotes
-- Note that this implementation also permits the strings to span multiple lines.
singleLineString :: Parser String
singleLineString = do
  skipped $ char '"'

  str <- many $ (notFollowedBy (char '"')) >> anyChar
  char '"'
  return $ read $ "\"" ++ str ++ "\""

varList :: Parser [Var]
varList = skipped $ var `sepBy` (skip >> char ';')


around :: Parser a -> Parser a
around p = many space >> p << many space

colon :: Parser Char
colon = char ':'

var :: Parser Var
var = do
  name <- skipped identifier
  around colon
  t <- skipped identifier
  return $ Var name t

assignExpr :: Parser Expression
assignExpr = do
  skip
  dest <- identifier
  skipped $ char '='
  exp <- skipped expression
  return $ Expr $ AssignVal dest exp

callExpr :: Parser Expression
callExpr = do
  skip
  name <- identifier
  char '.'
  args <- parens funArgs
  return $ Expr $ CallVal name args

parens :: Parser a -> Parser a
parens parser = do
  skipped $ char '('
  x <- parser
  skipped $ char ')'
  return x

funArgs :: Parser [Expression]
funArgs = do
  skipped $ expression `sepBy` (skipped $ char ',')


varExpr :: Parser Expression
varExpr = do
  ident <- identifier
  return $ Expr $ VarVal ident


block :: Parser Expression
block = do
  string "{"
  exprs <- blockExpressions
  skipped $ string "}"
  return $ Expr $ BlockVal exprs

blockExpressions :: Parser [Expression]
blockExpressions = do
  try (do
         firstExpr <- skipped expression
         exprs <- many $ try $ skipped $ semicolon >> expression
         return $ firstExpr:exprs)
      <|> return []


-- Infix operator parsing
orderTable :: OperatorTable Char () Expression
orderTable = [[Infix (binaryInfixOp "==" Equals) AssocLeft,
              --  Infix (binaryInfixOp "!=" NotEquals) AssocLeft,
               Infix (binaryInfixOp ">" Greater) AssocLeft,
               Infix (binaryInfixOp ">=" GreaterEq) AssocLeft,
               Infix (binaryInfixOp "<" Less) AssocLeft,
               Infix (binaryInfixOp "<=" LessEq) AssocLeft]]

binaryInfixOp :: String -> InfixOp -> Parser (Expression -> Expression -> Expression)
binaryInfixOp name op = do
  string name
  skip
  return $ \left right -> Expr $ (InfixVal op left right)

infixExpr = buildExpressionParser orderTable nonInfixExpr


-- Parse a semicolon
semicolon :: Parser Char
semicolon = skipped $ char ';'

-- Parse an identifier. These match [a-zA-Z0-9_]+
identifier :: Parser String
identifier = skipped $ many1 $ alphaNum <|> oneOf "_"

-- Wrap a parser in skips
skipped :: Parser a -> Parser a
skipped parser = do
  skip
  x <- parser
  skip
  return x

-- Skip anything that isn't code: whitespace and comments
skip :: Parser SourcePos
skip = do
  many $ whitespace
  getPosition

whitespace :: Parser String
whitespace = do
  c <- oneOf " \n\t"
  return [c]
