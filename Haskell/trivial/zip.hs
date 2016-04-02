zip' :: [a] -> [b] -> [(a,b)]
zip' (x:[]) (y:ys) = [(x, y)]
zip' (x:xs) (y:[]) = [(x, y)]
zip' (x:xs) (y:ys) = [(x, y)] ++ zip xs ys

list1 = [1, 2, 3, 4, 5]
list2 = ['a', 'b', 'c', 'd']

main = print (zip' list1 list2)
