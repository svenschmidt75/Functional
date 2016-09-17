module Lib
    ( Identity (..)
    ) where

import Data.Semigroup

newtype Identity a = Identity a
    deriving (Eq, Show)

instance Semigroup (Identity a) where
    (Identity a) <> (Identity _) = Identity a
