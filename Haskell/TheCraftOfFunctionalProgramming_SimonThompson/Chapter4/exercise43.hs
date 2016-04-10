module Exercise43 where

import Test.QuickCheck


howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual m n p
    | m == n && n == p = 3
    | m == n = 2
    | m == p = 2
    | n == p = 2
    | otherwise = 0

main :: IO ()
main = do
        print (howManyEqual 34 25 36)
        print (howManyEqual 34 25 34)
        print (howManyEqual 34 34 34)
