module Main where

printRevList :: [Integer] -> IO ()
printRevList list =
    printRevListTR list (putStrLn "Done!")
    where
        printRevListTR [] cont = cont

main :: IO ()
main = do
    printRevList []
