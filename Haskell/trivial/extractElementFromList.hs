head' :: [a] -> a
head' (x:_) = x

-- Extract nth elemwbt from list
-- n starts at 0
extract :: Int -> [a] -> a
extract n (x:xs)
	| n > 0  = extract (n - 1) xs
	| n == 0 = x

main = print (extract 3 [1, 2, 4, 7, 8])
