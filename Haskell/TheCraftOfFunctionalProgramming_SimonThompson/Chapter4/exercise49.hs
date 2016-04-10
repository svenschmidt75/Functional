module Exercise49 where

import Test.QuickCheck


maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)
maxThreeOccurs x y z = let lst = [x, y, z] in
                       let m = max (max x y ) z in
                       let cnt = length $ filter (\p -> m == p) lst in
                       (m, cnt)



main :: IO ()
main = do
        print (maxThreeOccurs 4 5 4)
