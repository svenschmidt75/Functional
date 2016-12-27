module Main where

import Lib


main :: IO ()
main = do
    putStrLn $ composed "Julie"
    putStrLn $ fmapped "Chris"
    print $ tupled "Julie"
    print $ tupled' "Julie"
    print $ tupled'' "Julie"
