module Main where

import Prelude hiding (product, and, or, reverse, unzip)
import qualified Prelude (product, and, or, reverse, unzip)

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

elemNum_primitives :: Integer -> [Integer] -> Integer
elemNum_primitives item xs = toInteger $ length $ filter (== item) xs

elemNum_listComprehension :: Integer -> [Integer] -> Integer
elemNum_listComprehension item xs = toInteger $ length $ [x | x <- xs, x == item]

-- exercise 7.9
unique :: [Integer] -> [Integer]
unique [] = []
unique xs = filter (\x -> elemNum x xs == 1) xs

unique_listComprehension :: [Integer] -> [Integer]
unique_listComprehension xs = [x | x <- xs, elemNum x xs == 1]

-- exercise 7.11
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

prop_reverse :: Eq a => [a] -> Bool
prop_reverse xs = reverse xs == Prelude.reverse xs

unzip :: [(a, b)] -> ([a], [b])
unzip [] = []
unzip ((x, y):xs) = 


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
    print $ elemNum_primitives 7 [1, 7, 5, 4, 3, 7, 8, 9, 7, 9]
    print $ elemNum_listComprehension 7 [1, 7, 5, 4, 3, 7, 8, 9, 7, 9]
    print $ unique [4, 2, 1, 3, 2, 3]
    print $ unique_listComprehension [4, 2, 1, 3, 2, 3]
    print $ reverse [1, 2, 3, 4]
    --quickCheck prop_reverse
