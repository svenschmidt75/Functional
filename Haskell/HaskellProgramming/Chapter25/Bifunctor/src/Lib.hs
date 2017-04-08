{-# LANGUAGE InstanceSigs #-}
module Lib
    ( Deux (..)
    ) where

import Data.Bifunctor


data Deux a b = Deux a b
    deriving (Show, Eq)
{-
class Bifunctor p where
{-# MINIMAL bimap | first, second #-}
bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
bimap f g = first f . second g
first :: (a -> b) -> p a c -> p b c
first f = bimap f id
second :: (b -> c) -> p a b -> p a c
second = bimap id
-}

instance Bifunctor Deux where
--    first :: (a -> b) -> p a c -> p b c
    first :: (a -> b) -> Deux a c -> Deux b c
    first f (Deux a c) = Deux (f a) c

--    second :: (b -> c) -> p a b -> p a c
    second :: (b -> c) -> Deux a b -> Deux a c
    second f (Deux a b) = Deux a (f b)
