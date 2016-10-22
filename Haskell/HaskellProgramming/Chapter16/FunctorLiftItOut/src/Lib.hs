module Lib
    ( LiftItOut (..)
    ) where

data LiftItOut f a = LiftItOut (f a)
    deriving (Eq, Show)

{- Example: LiftItOut Maybe Int
 - fmap f (LiftItOut (Just 1)) = LiftItOut (Just 2)
 - Hence: fmap f (Just 1) = Just (f 1)
 - => fmap f (LiftItOut a) = LiftItOut (fmap f a)
 - NOTE: a :: Maybe Int, hence we have to apply fmap again
 -- as we need to lift the function f into the Maybe Int context!!!
 -}
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut a) = LiftItOut (fmap f a)
