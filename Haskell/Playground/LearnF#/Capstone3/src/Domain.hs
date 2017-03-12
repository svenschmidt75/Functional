module Domain
     ( Account (..)
     , Customer (..)
     ) where

import qualified Data.UUID as DU
import Data.Decimal


newtype Customer = Customer { unCustomer :: String }
    deriving (Eq, Show)

data Account = Account { accountId :: DU.UUID
                       , owner     :: Customer
                       , balance   :: Decimal
                       }
    deriving (Show, Eq)