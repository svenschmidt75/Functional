module Exercise2 where

import Test.QuickCheck


-- ALT + F3: Select all occurances


exercise31 :: Bool -> Bool -> Bool
exercise31 x y
    | x == True  && y == False = True
    | x == False && y == True  = True
    | otherwise                = False

exercise33 :: Bool -> Bool -> Bool
exercise33 True False = True
exercise33 False True = True
exercise33 _ _ = False

prop_exercise31 :: Bool -> Bool -> Bool
prop_exercise31 x y = exercise31 x y == exercise33 x y

main = quickCheck prop_exercise31
