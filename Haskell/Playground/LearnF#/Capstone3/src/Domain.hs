module Domain
     ( Account (..)
     , Customer (..)
     , Command (..)
     , char2Command
     ) where

import qualified Data.UUID as DU
import qualified Data.Decimal as DD


newtype Customer = Customer { unCustomer :: String }
    deriving (Eq, Show)

data Account = Account { accountId :: DU.UUID
                       , owner     :: Customer
                       , balance   :: DD.Decimal
                       }
    deriving (Show, Eq)

data Command = Withdraw
             | Deposit
             | Exit
             deriving (Show, Eq)

char2Command :: Char -> Maybe Command
char2Command c
    | c == 'w'  = Just Withdraw
    | c == 'd'  = Just Deposit
    | c == 'x'  = Just Exit
    | otherwise = Nothing
