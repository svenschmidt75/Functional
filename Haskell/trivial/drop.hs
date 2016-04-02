drop' :: Int -> [a] -> [a]
drop' 0 xs  = xs
drop' n [] = []
drop' n (x:xs) = drop' (n - 1) xs

main = print (drop' 3 [1, 2, 3, 4, 5])
