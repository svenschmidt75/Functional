module Exercise48 where

import Test.QuickCheck


triArea :: Float -> Float -> Float -> Float
triArea a b c
    | possible  = sqrt (s*(s-a)*(s-b)*(s-c))
    | otherwise = 0
    where
        s = (a+b+c)/2
        possible = a > 0 && b > 0 && c > 0 && triInequality
        triInequality = let longest = max (max a b) c in
                        longest <= a + b && longest <= a + c && longest <= b + c

main :: IO ()
main = do
        let area1 = triArea 9 6 (sqrt (9 * 9 + 6 * 6))
        print area1
        let area2 = triArea 11 14 15
        print area2
        let area3 = triArea 30 22 28
        print area3
