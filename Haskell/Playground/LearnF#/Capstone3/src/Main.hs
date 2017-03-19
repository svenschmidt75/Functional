module Main where

import Data.Decimal
import qualified Transactions as T
import Operations (Command (BankCommand, Exit)
                 , BankOperation (Withdraw, Deposit)
                 , char2Command
                 , tryGetBankOperation
                 , withdraw
                 , deposit
                 , initializeAccount)
import Domain (Account (..)
             , Customer (..))
import Audit


runOperation :: BankOperation -> Account -> Decimal -> IO Account
runOperation operation account amount = do
    let name = unCustomer $ owner account
    let aid = accountId account
    let audit = case operation of
                    Withdraw -> auditAs "withdraw" (T.writeTransaction name aid)
                    Deposit  -> auditAs "deposit" (T.writeTransaction name aid)
    _ <- audit account amount
    return $ case operation of
        Withdraw -> withdraw account amount
        Deposit  -> deposit account amount

mainLoop :: Account -> IO Account
mainLoop account = do
    putStrLn "(d)eposit, (w)ithdraw or e(x)it: "
    op <- char2Command <$> getChar
    case op of
        Nothing -> do
                    putStrLn "Unknown command. Please Try again"
                    mainLoop account
        Just cmd ->
            if cmd == Exit then
                return account
            else do
                putStrLn "Amount: "
                amount <- read <$> getLine
                let bank_operation = tryGetBankOperation cmd
                newAccount <- case bank_operation of
                                Just op -> runOperation op account amount
                                _       -> return account
                print newAccount
                mainLoop newAccount

main :: IO ()
main = do
    putStrLn "Please enter your name: "
    owner <- getLine
    (guid, transactions) <- T.findTransactionsOnDisk owner
    let account = initializeAccount owner guid transactions
    -- Without these two lines I get
    -- openBinaryFile: resource busy (file is locked)
    -- when appending a transaction...
    print account
    print transactions
--    mapM_ (putStrLn . show) transactions
    newAccount <- mainLoop account
    print newAccount