module Lib
    ( BoolConj (..)
    )  where

import Data.Semigroup

newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True)  <> (BoolConj True) = BoolConj True
    _                <> _               = BoolConj False
