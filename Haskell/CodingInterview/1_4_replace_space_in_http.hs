replace_space :: [Char] -> [Char]
replace_space [] = []
replace_space (x:xs)
	| x == ' '  = "%20" ++ replace_space xs
	| otherwise = [x] ++ replace_space xs

main = print (replace_space "Haskell is fun, but hard")
