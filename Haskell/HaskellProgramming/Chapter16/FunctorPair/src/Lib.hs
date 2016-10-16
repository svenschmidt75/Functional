module Lib
    ( Pair (..)
    ) where

data Pair a b = Pair a b

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)
