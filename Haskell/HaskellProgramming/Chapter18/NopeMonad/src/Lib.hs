module Lib
    ( Nope (..)
    ) where

data Nope a = NopeDotJpg
    deriving (Eq, Show)

instance Functor Nope where
    fmap _ NopeDotJpg  = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg

-- (<*>) :: f      (b -> c) -> f      b -> f      c
-- (<*>) :: Nope a (b -> c) -> Nope a b -> Nope a c
    (<*>) _ NopeDotJpg = NopeDotJpg

instance Monad Nope where
    return = pure
-- (>>=) :: m     x -> (x -> m     y) -> m     y
-- (>>=) :: Nope a x -> (x -> Nope a y) -> Nope a y
    (>>=) NopeDotJpg _ = NopeDotJpg
