module Lib
    ( List (..)
    ) where

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil        = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)
