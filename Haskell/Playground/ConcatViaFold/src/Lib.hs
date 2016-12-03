module Lib
    ( concat'
    ) where

concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = foldl (++) [] xs
