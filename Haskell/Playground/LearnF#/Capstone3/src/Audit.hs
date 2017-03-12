module Audit
     ( auditAs ) where

import Data.Decimal
import Data.Time.Clock
import qualified Transactions as T
import Domain


auditAs :: String -> (T.Transaction -> IO ()) -> Account -> Decimal -> IO ()
auditAs operation transactionWriter account amount = do
    let accepted = case operation of
                    "withdraw" -> balance account - amount >= 0
                    _          -> True
    timestamp <- getCurrentTime
    transactionWriter $ T.Transaction timestamp operation amount accepted
