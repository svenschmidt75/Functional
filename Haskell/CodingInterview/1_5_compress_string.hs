count_element :: Char -> [Char] -> Int
count_element c cs
	| null cs      = 0
	| c == head cs = count_element c (tail cs) + 1
	| otherwise    = 0

skip :: Int -> [a] -> [a]
skip n (x:xs)
	| n > 0 = skip (n - 1) xs
	| otherwise = [x] ++ xs

compress_string :: [Char] -> [Char]
compress_string [] = []
compress_string (c:cs) = [c] ++ show ((count_element c cs) + 1) ++ compress_string (skip (count_element c cs) cs)

main = print (compress_string "aabcccccaaa")
