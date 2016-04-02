move_lowest_element_to_front :: (Ord a) => [a] -> [a]
move_lowest_element_to_front [] = []
move_lowest_element_to_front [n] = [n]
move_lowest_element_to_front (x:xs)
	| x > (head (move_lowest_element_to_front xs))     = [head xs] ++ x ++ tail xs
    | otherwise = [x] ++ move_lowest_element_to_front xs

main = print (move_lowest_element_to_front [1,2])
