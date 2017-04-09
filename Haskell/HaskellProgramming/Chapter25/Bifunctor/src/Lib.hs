{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( Deux (..)
    , Const (..)
    , Drei (..)
    , SuperDrei (..)
    , SemiDrei (..)
    , Quadriceps (..)
    , MyEither (..)
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





data SuperDrei a b c = SuperDrei a b
    deriving (Show, Eq)

instance Bifunctor (SuperDrei a) where
--    first :: (a -> b) -> p a c -> p b c
    first :: (b1 -> b2) -> SuperDrei a b1 c -> SuperDrei a b2 c
    first f (SuperDrei a b1) = SuperDrei a (f b1)

--    second :: (b -> c) -> p a b -> p a c
    second :: (c1 -> c2) -> SuperDrei a b c1 -> SuperDrei a b c2
    second _ (SuperDrei a b) = SuperDrei a b




data SemiDrei a b c = SemiDrei a
    deriving (Show, Eq)

instance Bifunctor (SemiDrei a) where
--    first :: (a -> b) -> p a c -> p b c
    first :: (b1 -> b2) -> SemiDrei a b1 c -> SemiDrei a b2 c
    first _ (SemiDrei a) = SemiDrei a

--    second :: (b -> c) -> p a b -> p a c
    second :: (c1 -> c2) -> SemiDrei a b c1 -> SemiDrei a b c2
    second _ (SemiDrei a) = SemiDrei a




data Quadriceps a b c d = Quadzzz a b c d
    deriving (Show, Eq)

instance Bifunctor (Quadriceps a b) where
--    first :: (a -> b) -> p a c -> p b c
    first :: (c1 -> c2) -> Quadriceps a b c1 d -> Quadriceps a b c2 d
    first f (Quadzzz a b c1 d) = Quadzzz a b (f c1) d

--    second :: (b -> c) -> p a b -> p a c
    second :: (d1 -> d2) -> Quadriceps a b c d1 -> Quadriceps a b c d2
    second f (Quadzzz a b c d1) = Quadzzz a b c (f d1)


data MyEither a b = MyLeft a
                  | MyRight b
    deriving (Show, Eq)

instance Bifunctor MyEither where
--    first :: (a -> b) -> p a c -> p b c
    first :: (a1 -> a2) -> MyEither a1 b -> MyEither a2 b
    first f (MyLeft a)  = MyLeft (f a)
    first _ (MyRight b) = MyRight b

--    second :: (b -> c) -> p a b -> p a c
    second :: (b1 -> b2) -> MyEither a b1 -> MyEither a b2
    second f (MyRight b) = MyRight (f b)
    second _ (MyLeft a)  = MyLeft a
