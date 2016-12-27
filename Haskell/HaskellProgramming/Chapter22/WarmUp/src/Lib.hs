module Lib where

import Data.Char


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

-- using applicative
-- two functions, each takes one argument
-- the result is combined using another function
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    r1 <- rev
    r2 <- cap
    return (r1, r2)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = rev >>= \r1 -> cap >>= \r2 -> return (r1, r2)
