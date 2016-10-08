module Lib
    ( Trivial (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty  = Trivial
    mappend = (<>)
