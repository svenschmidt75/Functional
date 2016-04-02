factorial :: (Num a) => Int -> Int
factorial n
		| n == 0    = 1
		| otherwise = n * factorial (n - 1)

main = print (factorial 31)
