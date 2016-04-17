module Exercise520 where

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n
    | n == 1 = True
    | otherwise = if length divs == 2 then
                    let (x:y:_) = divs in
                    x == 1 && y == n
                  else
                      False
                  where
                    divs = divisors n

main :: IO ()
main = do
    print (isPrime 1)
