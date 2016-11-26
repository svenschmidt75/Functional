module Lib
    ( Three (..)
    ) where

import Data.Monoid


data Three a b c = Three a b c
    deriving (Show, Eq)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure c                        = Three mempty mempty c
    (Three a1 b1 f) <*> (Three a2 b2 c) = Three (a1 <> a2) (b1 <> b2) (f c)
