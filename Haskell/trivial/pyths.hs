pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n], x*x + y*y == n*n]

main = print (pyths 5)
