module Exercise432 where

import Test.QuickCheck hiding (Result)



{-
  2^n: 

  n even: 2^n = 2^(2*m) = (2^m)^2
  n odd:  2^n = 2^(2*m+1) = (2^m)^2 * 2
-}

raise_by_2 :: Integer -> Integer
raise_by_2 n = if n `mod` 2 == 0 then
                 let m = n `div` 2 in
                 raise_by_2_even m * raise_by_2_even m
               else
                 let m = (n - 1) `div` 2 in
                 raise_by_2_even m * raise_by_2_even m * 2
               where
                 raise_by_2_even m
                  | m == 0 = 1
                  | otherwise = 2 * raise_by_2_even (m - 1)

main :: IO ()
main = do
        print (raise_by_2 3)
