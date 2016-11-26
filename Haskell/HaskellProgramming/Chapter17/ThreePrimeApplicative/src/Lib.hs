module Lib
    ( Three' (..)
    ) where

import Data.Monoid


data Three' a b = Three' a b b
    deriving (Show, Eq)

instance Functor (Three' a) where
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Monoid a => Applicative (Three' a) where
    pure b                                  = Three' mempty b b
    (Three' a1 f1 f2) <*> (Three' a2 b1 b2) = Three' (a1 <> a2) (f1 b1) (f2 b2)
