{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( Deux (..)
    , Const (..)
    , Drei (..)
    ) where

import Data.Bifunctor


data Deux a b = Deux a b
    deriving (Show, Eq)

-- due to FlexibleInstances
instance Show (Int -> Int) where
    show :: (Int -> Int) -> String
    show _ = "Int -> Int"

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




data Const a b = Const a
    deriving (Show, Eq)

instance Bifunctor Const where
--    first :: (a -> b) -> p a c -> p b c
    first :: (a -> b) -> Const a c -> Const b c
    first f (Const a) = Const (f a)

--    second :: (b -> c) -> p a b -> p a c
    second :: (b -> c) -> Const a b -> Const a c
    second _ (Const a) = Const a



data Drei a b c = Drei a b c
    deriving (Show, Eq)

instance Bifunctor (Drei a) where
--    first :: (a -> b) -> p a c -> p b c
    first :: (b1 -> b2) -> Drei a b1 c -> Drei a b2 c
    first f (Drei a b1 c) = Drei a (f b1) c

--    second :: (b -> c) -> p a b -> p a c
    second :: (c1 -> c2) -> Drei a b c1 -> Drei a b c2
    second f (Drei a b c1) = Drei a b (f c1)
