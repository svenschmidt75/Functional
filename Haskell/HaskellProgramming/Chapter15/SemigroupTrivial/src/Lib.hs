module Lib
    ( Trivial (..)
    ) where

import Data.Semigroup

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial
