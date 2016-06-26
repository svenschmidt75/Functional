module Main where

import Test.QuickCheck

putNtimes :: Integer -> String -> IO ()
putNtimes 0 st = return ()
putNtimes n st = do
    putStrLn st
    putNtimes (n - 1) st

main :: IO ()
main = do
    putNtimes 5 "Test"