module Operations
     ( Command (..)
     , BankOperation (..)
     , char2Command
     , tryGetBankOperation
     , initializeAccount
     , deposit
     , withdraw
     ) where

import qualified Data.UUID as DU
import Data.Decimal
import Domain
import Transactions


data BankOperation = Deposit
                   | Withdraw
    deriving (Show, Eq)

data Command = BankCommand BankOperation
             | Exit
    deriving (Show, Eq)

char2Command :: Char -> Maybe Command
char2Command c
    | c == 'w'  = Just $ BankCommand Withdraw
    | c == 'd'  = Just $ BankCommand Deposit
    | c == 'x'  = Just Exit
    | otherwise = Nothing

tryGetBankOperation :: Command -> Maybe BankOperation
tryGetBankOperation (BankCommand op) = Just op
tryGetBankOperation _                = Nothing

initializeAccount :: String -> DU.UUID-> [Transaction] -> Account
initializeAccount owner guid transactions =
    Account guid (Customer owner) amount
    where
        amount = foldr f 0 transactions
        f (Transaction _ op amount accepted) accum =
            if accepted then
                case op of
                    "deposit"  -> accum + amount
                    "withdraw" -> accum - amount
            else
                accum

deposit :: Account -> Decimal -> Account
deposit (Account id owner balance) amount = Account id owner (balance + amount)

withdraw :: Account -> Decimal -> Account
withdraw (Account id owner balance) amount = Account id owner (balance - amount)
