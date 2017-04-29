module Main where

import Transformers
import qualified Data.Map as Map


example0 :: Value
example0 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in eval0 Map.empty exampleExp

example1 :: Value
example1 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval1 (eval1 Map.empty exampleExp)

example2a :: Either String Value
example2a =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval2 (eval2a Map.empty exampleExp)

example2b_1 :: Either String Value
example2b_1 = runEval2 (eval2b Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

example2b_2 :: Either String Value
example2b_2 = runEval2 (eval2b Map.empty (Var "x"))

main :: IO ()
main = do
    print example0
    print example1
    print example2a
    print example2b_1
    print example2b_2
