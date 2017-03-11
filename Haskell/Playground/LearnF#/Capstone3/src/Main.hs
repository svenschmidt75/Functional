module Main where

import qualified Transactions as T


main :: IO ()
main = do
  transactions <- T.findTransactionsOnDisk "Sven"
  mapM_ (putStrLn . show) transactions
  -- print $ T.deserialize "Account 6e32b3e6-44c9-44d7-ac04-b6e0c5bb6e13: 3/11/17 6:22:47 AM***deposit***120***true"
  -- mguid <- T.getGuid "/tmp/audits/Sven/6e32b3e6-44c9-44d7-ac04-b6e0c5bb6e13.txt"
  -- print mguid