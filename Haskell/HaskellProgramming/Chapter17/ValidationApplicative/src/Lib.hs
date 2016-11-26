module Lib
    ( Validation (..)
    ) where

data Validation e a = Failure e
                    | Success a
    deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure = undefined
    (<*>) = undefined
