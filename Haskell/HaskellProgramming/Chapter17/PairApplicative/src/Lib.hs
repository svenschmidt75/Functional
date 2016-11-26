module Lib
    ( Pair (..)
    ) where


data Pair a = Pair a a
    deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

instance Applicative Pair where
    pure a                        = Pair  a a
    (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)
