module Main where

import Data.Decimal
import qualified Transactions as T
import Operations
import Domain


isValidCommand :: Char -> Bool
isValidCommand x = x `elem` "dwx"

mainLoop :: Account -> IO Account
mainLoop account = do
    putStrLn "(d)eposit, (w)ithdraw or e(x)it: "
    op <- getChar
    if isValidCommand op then
        if op == 'x' then
            return account
        else do
            putStrLn "Amount: "
            amount <- read <$> getLine :: IO Decimal
            mainLoop $ runOperation op account amount
    else
        mainLoop account
    where
        runOperation 'd' account amount = deposit account amount
        runOperation 'w' account amount = withdraw account amount


main :: IO ()
main = do
    putStrLn "Please enter your name: "
    owner <- getLine
    (guid, transactions) <- T.findTransactionsOnDisk owner
    let account = initializeAccount owner guid transactions
    print account
--    mapM_ (putStrLn . show) transactions
    newAccount <- mainLoop account
    print newAccount