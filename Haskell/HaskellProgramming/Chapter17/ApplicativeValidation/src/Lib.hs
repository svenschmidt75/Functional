module Lib
    ( Validation (..)
    ) where

data Validation e a = Failure e
                    | Success a
    deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

instance Monoid e => Applicative (Validation e) where
-- f ~ Validation e
-- pure :: a -> f            a
-- pure :: a -> Validation e a
    pure  a = Success a
-- <*> :: f            (a -> b) -> f            a -> f            b
-- <*> :: Validation e (a -> b) -> Validation e a -> Validation e b
    (<*>) (Failure e1) (Failure e2) = Failure $ e1 `mappend` e2
    (<*>) (Failure e)  _            = Failure e
    (<*>) _            (Failure e)  = Failure e
    (<*>) (Success f)  (Success a)  = Success $ f a
