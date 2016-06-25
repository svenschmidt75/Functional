module Main where

import Prelude hiding (drop, splitAt)
import qualified Prelude (drop, splitAt)

import Test.QuickCheck


-- Exercise 7.25
is_sublist :: String -> String -> Bool
is_sublist [] _  = False
is_sublist es [] = False
is_sublist (e:es) (s:ss)
    | e == s    = is_sublist' es ss
    | otherwise = is_sublist (e:es) ss
        where
            is_sublist' [] _  = True
            is_sublist' es [] = False
            is_sublist' (e:es) (s:ss) 
                | e == s    = is_sublist' es ss
                | otherwise = is_sublist' (e:es) ss

main :: IO ()
main = do
    print $ is_sublist "ship" "Fish & Chips"
    print "Done"