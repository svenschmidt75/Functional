module Main where

import Transformers
import qualified Data.Map as Map


example0 :: Value
example0 =
    let exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
    in eval0 Map.empty exampleExp

main :: IO ()
main = do
    print $ example0
