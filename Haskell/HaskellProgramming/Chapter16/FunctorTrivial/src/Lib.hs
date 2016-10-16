module Lib
    ( Trivial (..)
    ) where

data Trivial = Trivial
    deriving (Eq, Show)

-- SS: Cannot implement functor instance for Trivial because
-- the data type Trivial has kindness *, but functor requires
-- kindness * -> *.
-- This is because with kindness *, there is no argument that
-- we could apply the function f to...
--instance Functor Trivial where
--    fmap f Trivial = Trivial
