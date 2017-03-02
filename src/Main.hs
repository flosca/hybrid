module Main where

import Parser
import System.IO
import System.Environment


ruleExample :: String
ruleExample = "rule withdrawal (ac: ACCOUNT; n, initial_amount: INTEGER): \
              \ {ac.amount = initial_amount; ac.amount <= n} {ac.withdraw (n)} {ac.amount = initial_amount - n}"

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ readExpr expr
