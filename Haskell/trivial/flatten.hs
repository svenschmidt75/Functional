flatten :: [[a]] -> [a]
flatten xss = [x | xs <- xss, x <- xs]

main = print (flatten [[10], [12]])
