module Operations
     ( initializeAccount
     , deposit
     , withdraw
     ) where

import qualified Data.UUID as DU
import Data.Decimal
import Domain
import Transactions


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
