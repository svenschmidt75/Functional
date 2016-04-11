module Exercise417 where

import Test.QuickCheck hiding (Result)

rangeProduct :: Integer -> Integer -> Integer
rangeProduct m n
  | m > n     = 0
  | m < n     = m * rangeProduct (m + 1) n
  | otherwise = n

fac :: Integer -> Integer
fac n
  | n == 0 = 1
  | n > 0  = n * fac (n - 1)

fac2 :: Integer -> Integer
fac2 n = rangeProduct 1 n


prop_exercise417 :: Integer -> Bool
prop_exercise417 n = fac2 ( abs n + 1) == fac (abs n + 1)

main :: IO ()
main = do
        print (rangeProduct 3 2)
        quickCheck prop_exercise417
