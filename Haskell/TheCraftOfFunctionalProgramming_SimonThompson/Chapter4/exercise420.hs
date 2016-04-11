module Exercise420 where

import Test.QuickCheck hiding (Result)

integerSquareRoot :: Integer -> Integer
integerSquareRoot n = integerSquareRootHelper n 1

integerSquareRootHelper :: Integer -> Integer -> Integer
integerSquareRootHelper n m
    | m * m <= n = integerSquareRootHelper n (m + 1)
    | otherwise  = m - 1

main :: IO ()
main = do
        print (integerSquareRoot 25)
