module Exercise2 where

exercise31_1 :: Bool -> Bool -> Bool
exercise31_1 x y
    | x == True && y == False = True
    | x == False && y == True = True
    | otherwise = False
