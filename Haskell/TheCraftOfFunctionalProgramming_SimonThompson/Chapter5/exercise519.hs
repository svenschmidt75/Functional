module Exercise519 where

import Test.QuickCheck hiding (Result)
import Data.Char


capitalize :: String -> String
capitalize input = [toUpper x | x <- input]

capitalizeCharOnly :: String -> String
capitalizeCharOnly input = [toUpper x | x <- input, isLetter x]

main :: IO ()
main = do
    print (capitalize "abcdEFG")
    print (capitalizeCharOnly "ab1cdE!F.G")
