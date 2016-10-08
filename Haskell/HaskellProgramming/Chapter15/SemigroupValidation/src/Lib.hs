module Lib
    ( Validation' (..)
    ) where

import Data.Semigroup

data Validation' a b = Failure' a | Success' b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation' a b) where
    (Failure' a) <> (Failure' b) = Failure' $ a <> b
    (Failure' a) <> (Success' _) = Failure' a
    (Success' a) <> (Failure' b) = Failure' b
    (Success' a) <> (Success' _) = Success' a
