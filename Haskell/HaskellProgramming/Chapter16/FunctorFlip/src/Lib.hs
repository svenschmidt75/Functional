{-# LANGUAGE FlexibleInstances #-}

module Lib
    ( Flip (..)
    , K (..)
    ) where

{- Note: f is NOT a function and (f b a) is NOT function application.
 - This is because in both type and data constructors, only TYPES
 - show up, never values!!!
 - Hence, since (f b a) nust be a concrete type, f has kindness
 - * -> * -> *. Either satisfies this constraint...
 - For example, this type takes an Either String Int and redefines
 - it as Either Int String.
 -}
newtype Flip f a b = Flip (f b a)
    deriving (Eq, Show)

newtype K a b = K a
    deriving (Eq, Show)

{- K can be used for f in Flip, because K has the same kindness as f,
 - * -> * -> *.
 -}
instance Functor (Flip K a) where
    fmap f (Flip a) = undefined

instance Functor (Flip Either a) where
    fmap f (Flip (Left a)) = Flip $ (Left (f a))
    fmap f (Flip (Right a)) = Flip $ (Right a)
