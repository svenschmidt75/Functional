module Lib
    ( Combine (..)
    )  where

import Data.Semigroup

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> f <> g $ n
