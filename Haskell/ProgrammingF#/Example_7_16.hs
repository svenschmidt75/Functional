module Main where

{- Programming in F# 3.0, Chris Smith
 - Example 7.16: Using continuations to implement operating
 - on a list in a tail-recursive fashion to avoid stack
 - overflow.
 - Translated to Haskell.
-}

printRevList :: [Integer] -> IO ()
printRevList list =
    printRevListTR list (putStrLn "Done!")
    where
        printRevListTR []     cont = cont
        printRevListTR (x:xs) cont = printRevListTR xs (do putStrLn $ show x;
                                                           cont)

main :: IO ()
main = do
    printRevList [1..10]
