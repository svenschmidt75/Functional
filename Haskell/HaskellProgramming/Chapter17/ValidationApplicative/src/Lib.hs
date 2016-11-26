module Lib
    ( Validation (..)
    ) where

import Data.Monoid


data Validation e a = Failure e
                    | Success a
    deriving (Eq, Show)

-- same as Either
instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success $ f a

-- This is different
instance Monoid e => Applicative (Validation e) where
    pure a                        = Success a
    (Success f)  <*> (Success a)  = Success $ f a
    (Failure e1) <*> (Failure e2) = Failure $ e1 <> e2
    (Success _)  <*> (Failure e)  = Failure $ e
    (Failure e)  <*> (Success _)  = Failure $ e
