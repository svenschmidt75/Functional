module Lib where

import Prelude hiding (foldMap)
import Data.Monoid ((<>))

-- foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b
-- fold :: Data.Monoid.Monoid m => t m -> m

sum :: (Foldable t, Num a) => t a -> a
sum x = foldr (+) 0 x

product :: (Foldable t, Num a) => t a -> a
product x = foldr (*) 1 x

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = foldr (\a b -> a == x || b) False xs

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = foldr min' Nothing xs
             where
               min' a Nothing  = Just a
               min' a (Just b) = if compare a b == GT
                                 then Just b
                                 else Just a

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs = foldr max' Nothing xs
             where
               max' a Nothing  = Just a
               max' a (Just b) = if compare a b == GT
                                 then Just a
                                 else Just b

null :: (Foldable t) => t a -> Bool
null xs = isEmpty $ foldr (:) [] xs
        where
          isEmpty [] = True
          isEmpty _  = False

length :: (Foldable t) => t a -> Int
length xs = foldr (\_ b -> b + 1) 0 xs

toList :: (Foldable t) => t a -> [a]
toList xs = foldr (:) [] xs

fold :: (Foldable t, Monoid m) => t m -> m
-- id works here because the argument of fold contains monoids,
-- so we don't have to map them to monoids, but to itself instead!
fold xs = foldMap id xs

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldr (\a b -> f a <> b) mempty xs
