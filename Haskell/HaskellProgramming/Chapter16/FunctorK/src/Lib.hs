module Lib
    ( K (..)
    ) where

data K a b = K a
    deriving (Eq, Show)

instance Functor (K a) where
-- we cannot define fmap as 'fmap f (K a) = K (f a)',
-- because the type signature of fmap is
-- fmap :: a -> b -> f a -> f b, and f :: K a, i.e.
-- we have to act on type b in 'K a b', but b is not
-- manifested in any value of type 'K a b'...
    fmap _ (K a) = K a
