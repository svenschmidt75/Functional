module Lib
    ( BoolDisj (..)
    )  where

import Data.Semigroup

newtype BoolDisj = BoolDisj Bool
    deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _               = BoolDisj True
    _               <> (BoolDisj True) = BoolDisj True
    _               <> _               = BoolDisj False
