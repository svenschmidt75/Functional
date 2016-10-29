module Lib
    ( Identity (..)
    ) where

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
-- f ~ Identity
-- Applicative f =>

-- pure :: a -> f        a
-- pure :: a -> Identity a
    pure = Identity

-- (<*>) :: f        (a -> b) -> f        a -> f        b
-- (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (<*>) (Identity f) (Identity a) = Identity $ f a
