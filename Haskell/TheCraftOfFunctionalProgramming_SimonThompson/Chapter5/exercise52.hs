module Exercise52 where

import Test.QuickCheck hiding (Result)


maxThree :: Integer -> Integer -> Integer -> Integer
maxThree a b c = max (max a b) c

minThree :: Integer -> Integer -> Integer -> Integer
minThree a b c = min (min a b) c

between :: Integer -> Integer -> Integer -> Bool
between a b c = a < b && c > b || a > b && c < b

middleThree :: Integer -> Integer -> Integer -> Integer
middleThree x y z
  | between y x z = x
  | between x y z = y
  | otherwise     = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (minThree x y z, middleThree x y z, maxThree x y z)

main :: IO ()
main = do
        print (maxThree 1 1 2)
        print (minThree 1 1 2)
        print (middleThree 3 2 4)
        print (middleThree 2 3 4)
        print (middleThree 2 4 3)
        print (orderTriple (2, 4, 3))
        print (orderTriple (5, 4, 1))
