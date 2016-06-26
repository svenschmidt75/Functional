module Main where

import Test.QuickCheck

main :: IO ()
main = do
    putStr "Please enter the 1st integer: "
    e1 <- getLine
    putStr "Please enter the 2nd integer: "
    e2 <- getLine
    let i1 = (read e1) :: Int
    let i2 = (read e2) :: Int
    putStrLn ("The sum of both is: " ++ show (i1 + i2))
