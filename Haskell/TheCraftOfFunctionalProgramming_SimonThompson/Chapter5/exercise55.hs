module Exercise55 where

import Test.QuickCheck hiding (Result)


data Shape = Circle Float
           | Rectangle Float Float
           deriving (Eq, Ord, Show)

perimeter_length :: Shape -> Float
perimeter_length (Circle radius) = 4.0 * diameter
                                   where
                                    diameter = 2.0 * radius
perimeter_length (Rectangle width height) = 2.0 * (width + height)


main :: IO ()
main = do
        print (perimeter_length (Circle 0.1))
        print (perimeter_length (Rectangle 1.0 2.0))
