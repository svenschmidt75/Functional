scalar_product :: (Num a) => [a] -> [a] -> [a]
scalar_product xs ys = [x*y | x <- xs, y <- ys]

-- NOTE!!!
-- 
scalar_product2 :: (Num a) => [a] -> [a] -> a
scalar_product2 xs ys = sum [x*y | (x,y) <- zip xs ys]

vector1 = [1, 2]
vector2 = [3, 4]

main = print (scalar_product2 vector1 vector2)
