module Exercise526 where

import Data.List


pushRight :: Int -> String -> String
pushRight linelength s = let len = length s in
                            take (linelength - len) (repeat ' ') ++ s

fibs :: [Integer]
fibs = 1 : 1 : [a + b | (a, b) <- zip fibs (tail fibs)]

fib :: Integer -> Integer
fib n = genericIndex fibs n

fibTableOneLine :: Integer -> String
fibTableOneLine n = show n ++ (pushRight 12 (show (fib n))) ++ "\n"

fibTable :: Integer -> String
fibTable n = "n" ++ (pushRight 12 "fib n") ++ "\n" ++ [x | i <- [0..n], x <- fibTableOneLine i]

main :: IO ()
main = do
    putStrLn (fibTable 5)
