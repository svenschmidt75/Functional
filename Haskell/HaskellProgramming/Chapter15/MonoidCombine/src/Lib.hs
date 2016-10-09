module Lib
    ( Combine (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


newtype Combine a b = Combine { unCombine :: (a -> b) }
    deriving (Eq, Show)

instance Semigroup b => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> (f n) <> (g n)

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
    mempty  = Combine $ mempty
    mappend = (<>)
