module Lib
    ( Four' (..)
    ) where

import Data.Monoid


data Four' a b = Four' a a a b
    deriving (Show, Eq)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance Monoid a => Applicative (Four' a) where
    pure b                                  = Four' mempty mempty mempty b
    (Four' a11 a12 a13 f) <*> (Four' a21 a22 a23 b) = Four' (a11 <> a21) (a12 <> a22) (a13 <> a23) (f b)
