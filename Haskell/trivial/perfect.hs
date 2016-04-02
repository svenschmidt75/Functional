factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- A positive integer is perfect if it equals the sum
-- of all its factors, excluding the number itself.
perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], sum (factors x) - x == x]

-- [6,26,296]
main = print (perfect 500)
