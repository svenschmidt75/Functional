filter' :: (a -> Bool) -> [a] -> [a]
filter' pred [] = []
filter' pred (x:xs)
--	| pred x    = [x] ++ filter' pred xs
	| pred x    = x : filter' pred xs
	| otherwise = filter' pred xs

filter2' :: (a -> Bool) -> [a] -> [a]
filter2' pred xs = [x | x <- xs, pred x]

list1 = [1, 2, 3, 4, 5]

main = print (filter2' even list1)
