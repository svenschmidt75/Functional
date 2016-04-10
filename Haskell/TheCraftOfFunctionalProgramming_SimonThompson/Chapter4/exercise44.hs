module Exercise44 where

import Test.QuickCheck

howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer
howManyOfFourEqual m n p q
    | m == n && n == p = 3
    | m == n = 2
    | m == p = 2
    | n == p = 2
    | otherwise = 0

main :: IO ()
main = do
        print (howManyOfFourEqual 34 25 36 85)
        print (howManyOfFourEqual 34 25 33 34)
        print (howManyOfFourEqual 34 34 34 21)
        print (howManyOfFourEqual 34 34 34 34)
