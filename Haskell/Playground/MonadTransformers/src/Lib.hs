module Lib
    ( Identity (..)
    ) where

data Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)
