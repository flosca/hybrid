module Parser
where

import Util ((<<), fromRight)

import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr


tripleExample = "rule withdrawal \
            \ (ac: t; n, initial_amount: Integer) \
            \ {ac != null; amount = initial_amount} \
            \ withdraw.ac(n) \
            \ {amount = initial_amount - n}"


data Expression = Expr Value
                | None
                deriving (Show, Eq)

data Value = AssignVal String Expression
           | InfixVal InfixOp Expression Expression
           | CallVal String String [Expression]
           | VarVal String
           | BlockVal [Expression]
           | RuleVal String [Var] Expression Expression Expression
           | DeclareVal String Expression
           | NumVal Int
           | StrVal String
           | CharVal Char
           deriving (Show, Eq)

type Type = String

data Var = Var String Type
        deriving (Show, Eq)

data InfixOp = Equals | NotEquals | Greater | Less | LessEq | GreaterEq
             | Plus | Minus -- TODO other operations
            deriving (Show, Eq)

data HoareTriple = HoareTriple { tripleName :: String
                               , variables :: [Var]
                               , preconds :: Expression
                               , body :: Expression
                               , postconds :: Expression}
                  deriving Show

-- Top-level parser
parseTriple :: String -> String -> Either ParseError HoareTriple
parseTriple filename input = parse tripleExpr filename input

tripleExpr :: Parser HoareTriple
tripleExpr = do
  skip
  string "rule"
  tripleName <- identifier
  args <- parens varList
  preconds <- block
  body <- callExpr
  postconds <- block
  return $ HoareTriple tripleName args preconds body postconds


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
  let parsers = [parens expression, numExpr, declareExpr, callExpr,
                 charExpr, stringExpr, assignExpr, varExpr]
      attempts = map try parsers
  out <- foldl (<|>) (head parsers) (tail parsers)
  skip
  return out


-- ruleExpr :: Parser Expression
-- ruleExpr = do
--   skip
--   string "rule"
--   ruleName <- identifier
--   -- many space
--   args <- parens varList
--   preconds <- block
--   body <- callExpr
--   postconds <- block
--   return $ Expr $ RuleVal ruleName args preconds body postconds

numExpr :: Parser Expression
numExpr = do
  skip
  str <- many1 (oneOf "0123456789")
  return $ Expr $ NumVal $ fst (readDec str !! 0)

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

multiLineString :: Parser String
multiLineString = do
  skipped $ string "<<<"

  str <- many $ (notFollowedBy (string ">>>")) >> anyChar
  skipped $ string ">>>"
  return str

singleLineString :: Parser String
singleLineString = do
  skipped $ char '"'
  str <- many $ (notFollowedBy (char '"')) >> anyChar
  char '"'
  return $ read $ "\"" ++ str ++ "\""


around :: Parser a -> Parser a
around p = many space >> p << many space

colon :: Parser Char
colon = char ':'

var :: Parser [Var]
var = do
  names <- skipped $ identifier `sepBy` (skip >> char ',')
  around colon
  t <- skipped identifier
  return $ [Var name t | name <- names]

varList :: Parser [Var]
-- varList = skipped $ var `sepBy` (skip >> char ';')
varList = do
  vars <- var `sepBy` (skip >> char ';')
  return $ concat vars


declareExpr :: Parser Expression
declareExpr = do
  skip
  var <- identifier
  val <- try assignment <|> return None
  return $ Expr $ DeclareVal var val

assignment :: Parser Expression
assignment = do
  skipped $ char '='
  skipped expression

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
  method <- identifier
  args <- parens funArgs
  return $ Expr $ CallVal name method args

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
         exprs <- many $ try $ around semicolon >> expression
         return $ firstExpr:exprs)
      <|> return []

infixOperation :: String -> InfixOp -> Parser (Expression -> Expression -> Expression)
infixOperation name op = do
  string name
  skip
  return $ \left right -> Expr $ (InfixVal op left right)

-- Infix operator parsing
orderTable :: OperatorTable Char () Expression
orderTable = [[Infix (infixOperation "==" Equals) AssocLeft,
               Infix (infixOperation "!=" NotEquals) AssocLeft,
               Infix (infixOperation ">" Greater) AssocLeft,
               Infix (infixOperation ">=" GreaterEq) AssocLeft,
               Infix (infixOperation "<" Less) AssocLeft,
               Infix (infixOperation "<=" LessEq) AssocLeft]
               ,
               [Infix (infixOperation "+" Plus) AssocLeft,
                Infix (infixOperation "-" Minus) AssocLeft]
               ]
              --  [Infix (infixOperation "." Call) AssocRi]]

infixExpr = buildExpressionParser orderTable nonInfixExpr

-- Parse a semicolon
semicolon :: Parser Char
semicolon = skipped $ char ';'

-- Parse an identifier. These match [a-zA-Z0-9_]+
identifier :: Parser String
identifier = skipped $ many1 $ alphaNum <|> oneOf "_"

skipped :: Parser a -> Parser a
skipped parser = do
  skip
  x <- parser
  skip
  return x

skip :: Parser SourcePos
skip = do
  many $ whitespace
  getPosition

whitespace :: Parser String
whitespace = do
  c <- oneOf " \n\t"
  return [c]
