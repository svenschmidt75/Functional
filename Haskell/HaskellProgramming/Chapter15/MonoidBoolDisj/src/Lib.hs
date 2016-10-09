module Lib
    ( BoolDisj (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj False) <> (BoolDisj False) = BoolDisj False
    _                <> _                = BoolDisj True

instance Monoid BoolDisj where
    mempty  = BoolDisj False
    mappend = (<>)
