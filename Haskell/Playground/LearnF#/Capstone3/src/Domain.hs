module Domain
     ( Account (..)
     , Customer (..)
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
