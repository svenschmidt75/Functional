module Exercise518 where

import Test.QuickCheck hiding (Result)


doubleAll :: [Integer] -> [Integer]
doubleAll xs = [2 * x | x <- xs]

main :: IO ()
main = do
    print (doubleAll [1, 2, 3, 4, 5, 6])
