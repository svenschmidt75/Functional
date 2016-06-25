module Main where

import Prelude hiding (drop, splitAt)
import qualified Prelude (drop, splitAt)

import Test.QuickCheck


-- Exercise 7.25
is_sublist :: String -> String -> Bool
is_sublist [] _  = False
is_sublist _  [] = False
is_sublist (e:es) (s:ss)
    | e == s    = is_sublist' es ss
    | otherwise = is_sublist (e:es) ss
        where
            is_sublist' [] _  = True
            is_sublist' es [] = False
            is_sublist' (e:es) (s:ss) 
                | e == s    = is_sublist' es ss
                | otherwise = is_sublist' (e:es) ss

is_subsequence :: String -> String -> Bool
is_subsequence [] _  = False
is_subsequence _  [] = False
is_subsequence (e:es) (s:ss)
    | e == s    = is_subsequence' es ss
    | otherwise = is_subsequence (e:es) ss
        where
            is_subsequence' [] _  = True
            is_subsequence' es [] = False
            is_subsequence' (e:es) (s:ss) 
                | e == s    = is_subsequence' es ss
                | otherwise = False

main :: IO ()
main = do
    print $ is_sublist "ship" "Fish & Chips"
    print $ is_subsequence  "Chip" "Fish & Chips"
    print $ is_sublist "shC" "Fish & Chips"
    print $ is_subsequence  "shC" "Fish & Chips"
    print "Done"
