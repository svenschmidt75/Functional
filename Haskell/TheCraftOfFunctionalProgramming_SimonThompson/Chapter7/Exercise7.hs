module Main where

import Prelude hiding (product, and, or, reverse, unzip, min, max)
import qualified Prelude (product, and, or, reverse, unzip, min, max)

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
unzip [] = ([], [])
unzip ((x, y):xs) = let (as, bs) = unzip xs in
                    ([x] ++ as, [y] ++ bs)

-- exercise 7.12

iSort :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y : ins x ys

min :: [Integer] -> Integer
min xs = let sorted = iSort xs in
         head sorted

min_noSort :: [Integer] -> Integer
min_noSort xs = min' seed xs
                where
                    seed = head xs
                    min' n [] = n
                    min' n (y:ys)
                        | n <= y    = min' n ys
                        | otherwise = min' y ys

max :: [Integer] -> Integer
max xs = let sorted = iSort xs in
         last sorted

max_noSort :: [Integer] -> Integer
max_noSort xs = max' seed xs
                where
                    seed = head xs
                    max' n [] = n
                    max' n (y:ys)
                        | n <= y    = max' y ys
                        | otherwise = max' n ys

-- exercise 7.14
isSorted :: [Integer] -> Bool
isSorted [] = True
isSorted (x:y:xs)
    | x <= y    = isSorted xs
    | otherwise = False
-- contains one element only
isSorted _ = True

prop_isSorted :: [Integer] -> Bool
prop_isSorted xs = isSorted $ iSort xs

-- exercise 7.16
ins_descending :: Integer -> [Integer] -> [Integer]
ins_descending x [] = [x]
ins_descending x (y:[])
    | x == y = [x]
    | otherwise = x:y:[]
ins_descending x (y:ys)
    | x < y     = x : ins_descending y ys
    | x > y     = y : ins_descending x ys
    | otherwise = ys

iSort_descending :: [Integer] -> [Integer]
iSort_descending []     = []
iSort_descending (x:xs) = ins_descending x (iSort_descending xs)


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
    print $ unzip [(1, 2), (3, 4)]
    print $ min [1, 7, 5, 4, 3, 7, -1, 8, 9, 7, 9]
    print $ max [1, 7, 5, 4, 3, 7, -1, 8, 9, 7, 9]
    print $ min_noSort [1, 7, 5, 4, 3, 7, -1, 8, 9, 7, 9]
    print $ max_noSort [1, 7, 5, 4, 3, 7, -1, 8, 9, 7, 9]
    print $ isSorted [1]
    print $ isSorted [1, 2, 3, 7, 9, 10]
    print $ isSorted $ reverse [1, 2, 3, 7, 9, 10]
    quickCheck prop_isSorted
    print $ iSort_descending [2, 1, 4, 1, 2]
