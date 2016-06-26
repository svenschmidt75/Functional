module Main where

import Test.QuickCheck

readNIntegers :: Integer -> IO [Integer]
readNIntegers 0 = return []
readNIntegers n = do
    putStr "Enter an integer: "
    i <- getLine
    let integer = (read i) :: Integer
    next <- readNIntegers (n - 1)
    return (integer : next)

main :: IO ()
main = do
    putStr "Please enter an integer: "
    e1 <- getLine
    let i1 = (read e1) :: Integer
    ints <- readNIntegers i1
    putStr "The sum of those integers is: "
    let summed = sum ints
    putStrLn $ show summed