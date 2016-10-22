module Lib
    ( IgnoreOne (..)
    ) where

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)
