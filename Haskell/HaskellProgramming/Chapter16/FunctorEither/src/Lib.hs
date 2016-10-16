module Lib
    ( Sum (..)
    ) where

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a)   = First a
    fmap f (Second a) = Second $ f a

{- We cannot apply f to First because that would change the
   underlying structure. The functor is the type (Sum a), and
   f a != a, hence the functor application would violate the
   functor laws!
-}