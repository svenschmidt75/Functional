module Exercise314 where

import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


min2 :: Int -> Int -> Int
min2 x y = if x < y then x else y

min3 ::  Int -> Int -> Int -> Int
min3 x y z = min2 x (min2 y z)

prop_exercise314 :: Int -> Int -> Int -> Bool
prop_exercise314 x y z = (min3 x y z) == (min x (min y z))

main = quickCheck prop_exercise314
