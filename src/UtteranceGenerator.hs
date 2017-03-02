module UtteranceGenerator
where

import Parser
import Util (fromRight)

import Data.List (intercalate)


testTriple :: HoareTriple
testTriple = fromRight $ parseTriple "hybrid" tripleExample

-- Rules for mapping triples to natural language
makeRule :: HoareTriple -> String
makeRule triple = ruleVars (variables triple) ++ " such that " ++ (ruleContracts $ preconds triple)

ruleVar :: Var -> String
ruleVar (Var name varType) = name ++ " of type " ++ varType

ruleVars :: [Var] -> String
ruleVars vars = "For any " ++ (intercalate ", " $ map ruleVar vars)

ruleContracts :: Expression -> String
ruleContracts contracts =
     (fromInfix  . fromExpr $ cs !! 0) ++ " and " ++ (fromDeclare . fromExpr $ cs !! 1)
      where cs = fromBlock . fromExpr $ contracts



-- ruleExpression exp =


-- Algebraic data type destructors
fromExpr (Expr val) = val
-- fromExpr (InfixVal op e1 e2) = fromInfix op e1 e2
-- fromExpr (DeclareVal val e) = fromDeclare val e
--
fromBlock (BlockVal val) = val
--
-- fromDeclare :: String -> Expression -> String
fromDeclare (DeclareVal val None) = val
fromDeclare (DeclareVal val e) = val ++ " is " ++ (fromDeclare . fromExpr $ e)
--
-- fromInfix (InfixVal NotEquals var1 var2) = var1 ++ " does not equal to " ++ var2
    --
fromInfix (InfixVal op e1 e2)
     | op == NotEquals = var1 ++ " does not equal to " ++ var2
     | op == Equals = var1 ++ " equals to " ++ var2
     | op == Greater = var1 ++ " is greater than " ++ var2
     | op == Less = var1 ++ " is lower than " ++ var2
     | op == Minus = var1 ++ " minus " ++ var2
     | op == Plus = var1 ++ " plus " ++ var2
    where var1 = fromDeclare . fromExpr $ e1
          var2 = fromDeclare . fromExpr $ e2
    --  | otherwise = ""
-- fromExpr (None) = None

-- fromValue :: Value a -> a
-- fromValue (BlockVal val) = val
-- fromValue (InfixVal val) = val
-- fromValue (AssignVal s exp)
-- fromValue InfixVal InfixOp Expression Expression
-- fromValue CallVal String String [Expression]
-- fromValue VarVal String
-- fromValue RuleVal String [Var] Expression Expression Expression
-- fromValue DeclareVal String Expression
-- fromValue NumVal Int
-- fromValue StrVal String
-- fromValue CharVal

-- fromInfixOp :: InfixOp a -> a
-- fromInfixOp (Equals )
