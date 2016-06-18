module Main where

import Prelude hiding (product, and, or)
import qualified Prelude (product, and, or)

import Test.QuickCheck


excercise71 :: Num a => [a] -> a
excercise71 []    = 0
excercise71 (x:_) = x + 1

excercise72 :: Num a => [a] -> a
excercise72 []      = 0
excercise72 (x:y:_) = x + y
excercise72 (x:[])  = x

excercise73_71 :: Num a => [a] -> a
excercise73_71 xs
    = if null xs then
        0
      else
        1 + head xs

--excercise73_72 :: Num a => [a] -> a

excercise74 :: String -> Char
excercise74 st = if (null st) then
                    '\0'
                 else
                    head st

-- exercise 7.5
product :: [Integer] -> Integer
product []     = 1
product (x:xs) = x * product xs

prop_exercise75 :: [Integer] -> Bool
prop_exercise75 xs = product xs == Prelude.product xs

-- exercise 7.6
-- flatten a list of bools to one bool
and :: [Bool] -> Bool
and []        = True
and (True:xs) = and xs
and (False:_) = False

prop_exercise76 :: [Bool] -> Bool
prop_exercise76 xs = and xs == Prelude.and xs

-- exercise 7.7
-- flatten a list of bools to one bool
or :: [Bool] -> Bool
or []         = False
or (x:xs)     = x || or xs

prop_exercise77 :: [Bool] -> Bool
prop_exercise77 xs = or xs == Prelude.or xs

-- exercise 7.8
elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum item (x:xs)
    | item == x = 1 + elemNum item xs
    | otherwise = elemNum item xs

main = do
    print $ excercise71 [0, 1]
    print $ excercise72 [1, 2]
    print $ excercise73_71 [1, 2]
    print $ excercise74 "763"
    print $ product [1, 2, 3, 4, 5, 6, 7, 8, 9]
    quickCheck prop_exercise75
    quickCheck prop_exercise76
    quickCheck prop_exercise77
    print $ elemNum 7 [1, 7, 5, 4, 3, 7, 8, 9, 7, 9]
