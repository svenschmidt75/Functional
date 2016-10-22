module Lib
    ( Parappa (..)
    ) where

data Parappa f g a = DaWrappa (f a) (g a)
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa a b) = DaWrappa (fmap f a) (fmap f b)
