module Lib where

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
--    Foldable t: foldr/foldMap available on t a
--  Monoid (f a): can monoidally combine f a
-- Applicative f: pure can be used on a

-- foldMap :: Monoid m => (a -> m) -> t a -> m
filterF p xs = foldMap (\a -> if p a then pure a else mempty) xs
