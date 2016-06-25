module Main where

import Prelude hiding (drop, splitAt)
import qualified Prelude (drop, splitAt)

import Test.QuickCheck


-- Exercise 7.20
drop :: Int -> [a] -> [a]
drop _ []     = []
drop 0 xs     = xs
drop n (x:xs) = drop (n - 1) xs

-- generate test data for QuickCheck
-- the 1st index is a positive number,
-- the 2nd return value is a list of Int
genArgs :: Gen(Int, [Int])
genArgs = do
    xs <- arbitrary
    i <- choose (0, 10000)
    return (i, xs)

prop_drop ::  Int -> [Int] -> Bool
prop_drop n xs = drop n xs == Prelude.drop n xs



splitAt :: Int -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt 0 xs = ([], xs)
splitAt n xs = splitAt' n [] xs
            where
                splitAt' 0 ls rs     = (ls, rs)
                splitAt' _ ls []     = (ls, [])
                splitAt' n ls (r:rs) = splitAt' (n - 1) (r:ls) rs

prop_splitAt ::  Int -> [Int] -> Bool
prop_splitAt n xs = splitAt n xs == Prelude.splitAt n xs

main = do
        quickCheck $ forAll genArgs $ \(i, xs) -> prop_drop i xs
        quickCheck $ forAll genArgs $ \(i, xs) -> prop_drop i xs
