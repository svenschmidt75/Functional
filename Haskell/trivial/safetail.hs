tail' :: [a] -> [a]
tail' xs
	| null xs   = []  -- null :: [a] -> Bool checks for an empty list
	| otherwise = tail xs


-- note: In pattern matching, only non-empty lists can be considered,
-- so
-- tail' [] = [] will not work!!!

show_list :: [a] -> String
show_list xs
	| null (tail' xs) = "empty"
	| otherwise       = "not empty"

main = print (show_list [])
