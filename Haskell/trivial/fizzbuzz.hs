module Main where

fizzbuzz :: Int -> String
fizzbuzz x = case (x `mod` 3 == 0, x `mod` 5 == 0) of
    (True, True)  -> "fizzbuzz"
    (True, False) -> "fizz"
    (False, True) -> "buzz"
    _             -> show x

main :: IO ()
main = do
    let input = [1..15]
    mapM_ (putStrLn . fizzbuzz) [1..15]