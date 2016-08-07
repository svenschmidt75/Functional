module Lib
    ( Expr (..)
    , eval
    , printExpr
    ) where


data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit a)     = a
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)
