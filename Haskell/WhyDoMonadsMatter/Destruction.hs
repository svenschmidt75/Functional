module Desctruction where

-- 1. print message to screen
-- 2. wait for user input
waitForInput :: String -> IO String
waitForInput s = do { putStrLn s ; getLine }

outputReverse :: String -> IO ()
outputReverse s = do { putStrLn (reverse s) }

main = do
        s <- waitForInput "Enter some text: "
        outputReverse s

-- want to write
--      main = ( outputReverse . waitForInput "Enter some text: ")
