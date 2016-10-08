module Lib
    ( AccumulateBoth (..)
    , Validation' (Failure', Success')
    ) where

import Data.Semigroup


data Validation' a b = Failure' a | Success' b
    deriving (Eq, Show)

newtype AccumulateBoth a b = AccumulateBoth (Validation' a b)
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
    (AccumulateBoth (Success' a)) <> (AccumulateBoth (Success' b)) = AccumulateBoth $ Success' $ a <> b
    (AccumulateBoth (Failure' a)) <> (AccumulateBoth (Failure' b)) = AccumulateBoth $ Failure' $ a <> b
    (AccumulateBoth (Failure' a)) <> _                             = AccumulateBoth $ Failure' $ a
    _                             <> (AccumulateBoth (Failure' a)) = AccumulateBoth $ Failure' $ a
