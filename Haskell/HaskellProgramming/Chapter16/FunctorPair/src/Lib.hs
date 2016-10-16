module Lib
    ( Pair (..)
    ) where

data Pair a b = Pair a b
    deriving (Eq, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)
