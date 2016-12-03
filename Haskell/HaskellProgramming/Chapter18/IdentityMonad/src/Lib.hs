module Lib
    ( Identity (..)
    ) where

data Identity a = Identity a
    deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a

-- (<*>) :: f          (b -> c) -> f          b -> f          c
-- (<*>) :: Identity a (b -> c) -> Identity a b -> Identity a c
    (<*>) (Identity f) (Identity a) = Identity $ f a

instance Monad Identity where
    return = pure
-- (>>=) :: m     x -> (x -> m     y) -> m     y
-- (>>=) :: Identity a x -> (x -> Identity a y) -> Identity a y
    (>>=) (Identity a) f = f a
