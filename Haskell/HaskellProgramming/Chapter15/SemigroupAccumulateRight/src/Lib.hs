module Lib
    ( AccumulateRight (..)
    , Validation' (Failure', Success')
    ) where

import Data.Semigroup

data Validation' a b = Failure' a | Success' b
    deriving (Eq, Show)

newtype AccumulateRight a b = AccumulateRight (Validation' a b)
    deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
    (AccumulateRight (Success' b1)) <> (AccumulateRight (Success' b2)) = (AccumulateRight (Success' $ b1 <> b2))
    (AccumulateRight (Success' _))  <> (AccumulateRight (Failure' a))  = (AccumulateRight (Failure' a))
    (AccumulateRight (Failure' a))  <> _                               = (AccumulateRight (Failure' a))
