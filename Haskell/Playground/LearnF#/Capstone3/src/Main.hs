module Main where

import Data.Decimal
import qualified Transactions as T
import Operations
import Domain
import Audit


isValidCommand :: Char -> Bool
isValidCommand x = x `elem` "dwx"

runOperation :: Char -> Account -> Decimal -> IO Account
runOperation operation account amount = do
    let name = unCustomer $ owner account :: String
    let aid = accountId account
    let audit = case operation of
                    'w' -> auditAs "withdraw" (T.writeTransaction name aid)
                    'd' -> auditAs "deposit" (T.writeTransaction name aid)
    audit account amount
    return $ case operation of
        'w' -> withdraw account amount
        'd' -> deposit account amount

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
            newAccount <- runOperation op account amount
            mainLoop newAccount
    else
        mainLoop account

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