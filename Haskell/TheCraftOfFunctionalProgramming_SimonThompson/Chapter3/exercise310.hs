module Exercise310 where

import Test.QuickCheck
import Exercise39 hiding (main)


-- ALT + F3: Select all occurances


fourEqual_1 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual_1 m n p q = m == n && n == p && p == q

threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = m == n && n == p

fourEqual_2 :: Integer -> Integer -> Integer -> Integer -> Bool
fourEqual_2 m n p q = threeEqual m n p && m == q

prop_exercise310 :: Integer -> Integer -> Integer -> Integer -> Bool
prop_exercise310 m n p q = (fourEqual_1 m n p q) == (fourEqual_2 m n p q)

main = quickCheck prop_exercise310
