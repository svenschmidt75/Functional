module Lib
    ( Two (..)
    ) where

import Data.Semigroup

data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)
