and' :: [Bool] -> Bool
and' (x:[]) = x == True
and' (x:xs) = x == True && and' xs

x :: Int
x = 10

main = print (and' [x > 0, mod x 5 == 0, x < 0])
