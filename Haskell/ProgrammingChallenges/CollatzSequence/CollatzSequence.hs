module Main where

-- https://www.reddit.com/r/dailyprogrammer_ideas/comments/4889s2/easy_collatz_cycles/

bla x
    | x `mod` 2 == 0 = x `div` 2
    | otherwise = 3 * x + 1

collatz_helper :: Integer -> Integer -> [Integer] -> (Integer, [Integer])
collatz_helper value count lst = if value == 1
                                    then (count, value : lst)
                                    else collatz_helper (bla value) (count + 1) (value : lst)

collatz :: Integer -> (Integer, [Integer])
collatz value = collatz_helper value 0 []

main = do
        putStrLn "Enter number: "
        s <- getLine
        let result = collatz (read s)
        putStrLn (show result)
