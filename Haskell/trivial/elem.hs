elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || elem' a xs

main = print (elem' 2 [3..10])
