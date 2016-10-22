module Lib
    ( EvilGoateeConst (..)
    ) where

data EvilGoateeConst a b = GoatyConst b
    deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)
