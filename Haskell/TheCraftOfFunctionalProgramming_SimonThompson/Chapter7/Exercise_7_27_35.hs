module Main where

import Test.QuickCheck


whitespace = ['\n', '\t', ' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x whitespace = []
    | otherwise         = x : getWord xs

dropWord :: String -> String


main :: IO ()
main = do
    print $ getWord " boo"
    print $ getWord "cat dog"
