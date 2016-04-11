module Exercise419 where

import Test.QuickCheck hiding (Result)

multiplication :: Integer -> Integer -> Integer
-- pos. and neg.
multiplication n m
    | m == 0    = 0
    | m >= 1    = n + multiplication n (m - 1)
    | m <= -1   = - n + multiplication n (m + 1)
    | otherwise = 1

prop_exercise419 :: Integer -> Integer -> Bool
prop_exercise419 m n = m * n == multiplication m n

main :: IO ()
main = do
        print (multiplication 5 (-3))
        quickCheck prop_exercise419
