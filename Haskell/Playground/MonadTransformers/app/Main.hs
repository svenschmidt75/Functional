module Main where

import Transformers
import Control.Monad (join)
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

example2c :: Either String Value
example2c = runEval2 (eval2c Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

example2 :: Either String Value
example2 = runEval2 (eval2 Map.empty (Plus (Lit 1) (Abs "x" (Var "x"))))

example3 :: Either String Value
example3 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    -- Note what happens here: eval3 no longer takes Map.empty as an argument!
    -- Instead, it is passed to runEval3!!!
    in runEval3 Map.empty (eval3 exampleExp)

example4 :: (Either String Value, Integer)
example4 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval4 Map.empty 0 (eval4 exampleExp)

example4' :: Either String (Value, Integer)
example4' =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval4' Map.empty 0 (eval4' exampleExp)

example5 :: ((Either String Value, [String]), Integer)
example5 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval5 Map.empty 0 (eval5 exampleExp)

example6 :: IO ((Either String Value, [String]), Integer)
example6 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in runEval6 Map.empty 0 (eval6 exampleExp)

main :: IO ()
main = do
    print example0
    print example1
    print example2a
    print example2b_1
    print example2b_2
    print example2c
    print example2
    print example3
    print example4
    print example4'
    print example5
    example6 >>= print
