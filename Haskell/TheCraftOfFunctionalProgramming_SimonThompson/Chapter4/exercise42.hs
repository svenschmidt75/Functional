module Exercise42 where

import Test.QuickCheck


between :: Integer -> Integer -> Integer -> Bool
between m n p = m <= n && p >= n

middleNumber :: Integer -> Integer -> Integer -> Integer
middleNumber m n p
    | between m n p = n
    | between n p m = p
    | otherwise     = m

main :: IO ()
main = do
        print (middleNumber 9 2 11)
