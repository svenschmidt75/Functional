module Lib
    ( Identity (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
    mempty  = Identity mempty
    mappend = (<>)
