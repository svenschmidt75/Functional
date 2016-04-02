select_nth' :: Int -> [a] -> a
select_nth' 0 (x:xs) = x
select_nth' n (x:xs) = select_nth' (n - 1) xs

main = print (select_nth' 2 [1..])
