module Lib
    ( Sum (..)
    ) where

data Sum a b = First a
             | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a)  = First a
    fmap f (Second b) = Second $ f b

instance Applicative (Sum a) where
    pure b                      = Second b

-- (<*>) :: f (b -> c) -> f b -> f c
-- (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
    (<*>) (Second f) (Second b) = Second $ f b
    (<*>) (First a)  _          = First a
    (<*>) _          (First a)  = First a

instance Monad (Sum a) where
    return = pure
    (>>=) = undefined
