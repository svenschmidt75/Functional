module Exercise51 where

import Test.QuickCheck hiding (Result)


maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs a b = if a == b then
                  (max a b, 2)
                else
                  (max a b, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)
maxThreeOccurs a b c = let (m1, i1) = maxOccurs a b in
                       let (m2, i2) = maxOccurs m1 c in
                       (max m1 m2, i1 + i2 - 1)

main :: IO ()
main = do
        print (maxOccurs 1 1)
        print (maxOccurs 3 2)
        print (maxThreeOccurs 1 2 3)
        print (maxThreeOccurs 1 2 1)
        print (maxThreeOccurs 2 1 2)
        print (maxThreeOccurs 2 3 3)
