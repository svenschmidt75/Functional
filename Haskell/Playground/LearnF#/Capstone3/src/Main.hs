module Main where

import qualified Transactions as T


main :: IO ()
main = do
  transactions <- T.findTransactionsOnDisk "Sven"
  mapM_ (putStrLn . show) transactions
