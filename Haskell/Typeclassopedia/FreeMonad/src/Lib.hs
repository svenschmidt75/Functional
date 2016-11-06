module Lib
    ( Free (..)
    ) where

data Free f a = Var a
              | Node (f (Free f a))

instance Functor (Free f) where
    fmap = undefined

instance Applicative (Free f) where
    pure = undefined
    (<*>) = undefined

instance Monad (Free f) where
    return = pure
    (>>=) = undefined
