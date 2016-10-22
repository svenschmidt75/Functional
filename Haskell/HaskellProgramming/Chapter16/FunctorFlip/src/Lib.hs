{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( Flip (..)
    ) where

-- flip the arguments to f
newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a

instance Functor (Flip K a) where
    fmap f (Flip a) = undefined
