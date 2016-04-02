bubblesort :: (Ord a, Num a) => [a] -> [a]
bubblesort [] = []
bubblesort [n] = [n]
bubblesort (x:y:xs)
	| x > y     = bubblesort ([y,x] ++ xs)
    | otherwise = [x] ++ bubblesort ([y] ++ xs)

main = print (bubblesort [2, 1, 8, 3, 2, 9, 6])
