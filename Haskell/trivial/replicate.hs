replicate' :: Int -> a -> [a]
replicate' 0 _  = []
replicate' n x = [x] ++ replicate' (n - 1) x

main = print (replicate' 10 'a')
