reverse_string :: [Char] -> [Char]
reverse_string [] = []
reverse_string (x:[]) = [x]
reverse_string (x:xs) = reverse_string xs ++ [x]

main = print (reverse_string "Haskell")
