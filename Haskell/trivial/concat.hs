concat' :: [[a]] -> [a]
{-
	Example:
	  Input: [[1,2], [3,4]]
	  Output: [1, 2, 3, 4]

	  i.e. concat flattens lists.
-}
concat' (xs:[]) = xs
concat' (xs:xss) = xs ++ concat' xss

main = print (concat' [[1, 2], [3, 4], [9, 7]])
