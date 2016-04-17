module Exercise521 where

import Prelude hiding (elem)

matches :: Integer -> [Integer] -> [Integer]
matches n ns = [x | x <- ns, x == n]

elem :: Integer -> [Integer] -> Bool
elem n ns = not (null (matches n ns))

main :: IO ()
main = do
    print (matches 1 [1, 2, 1, 4, 5, 1])
    print (matches 1 [2, 3, 4, 6])
    print (elem 1 [1, 2, 1, 4, 5, 1])
    print (elem 1 [2, 3, 4, 6])
