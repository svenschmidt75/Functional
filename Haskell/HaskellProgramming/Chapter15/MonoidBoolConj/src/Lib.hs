module Lib
    ( BoolConj (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True)  <> (BoolConj True) = BoolConj True
    (BoolConj True)  <> _               = BoolConj False
    (BoolConj False) <> _               = BoolConj False

instance Monoid BoolConj where
    mempty  = BoolConj True
    mappend = (<>)
