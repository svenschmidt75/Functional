module Lib
    ( Or (Fst, Snd)
    )  where

import Data.Semigroup

data Or a b = Fst a
            | Snd b
    deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd b) <> _       = (Snd b)
    (Fst a) <> (Snd b) = (Snd b)
    (Fst a) <> (Fst b) = (Fst b)
