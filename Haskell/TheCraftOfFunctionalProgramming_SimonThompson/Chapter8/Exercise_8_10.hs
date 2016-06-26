module Main where

import Data.Char

import Test.QuickCheck

ignore = ['\'', ' ', '\t']

is_palindrome :: String -> Bool
is_palindrome [] = False
is_palindrome word = is_palindrome' word (reverse word)
            where
                is_palindrome' [] [] = True
                is_palindrome' (w:ws) (d:ds)
                    | elem w ignore          = is_palindrome' ws     (d:ds)
                    | elem d ignore          = is_palindrome' (w:ws) ds
                    | toLower w == toLower d = is_palindrome' ws     ds
                    | otherwise              = False

main :: IO ()
main = do
    word <- getLine
    putStr "You enetered '"
    putStr word
    putStr "' which is "
    result <- case is_palindrome word of
        False -> return "not "
        True  -> return ""
    putStr result
    putStrLn "a palindrome."
