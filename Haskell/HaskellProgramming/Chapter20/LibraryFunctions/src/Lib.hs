module Lib where

--foldMap :: Data.Monoid.Monoid m => (a -> m) -> t a -> m
-- foldr :: (a -> b -> b) -> b -> t a -> b

sum :: (Foldable t, Num a) => t a -> a
sum x = foldr (+) 0 x

product :: (Foldable t, Num a) => t a -> a
product x = foldr (*) 1 x

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x xs = foldr (\a b -> a == x || b) False xs

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs = foldr min' Nothing xs
             where
               min' a Nothing = Just a
               min' a (Just b) = case compare a b of
                                   LT -> Just a
                                   EQ -> Just a
                                   GT -> Just b

{-
4. minimum :: (Foldable t, Ord a) => t a -> Maybe a
5. maximum :: (Foldable t, Ord a) => t a -> Maybe a
6. null :: (Foldable t) => t a -> Bool
7. length :: (Foldable t) => t a -> Int
8. Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
9. Hint: use foldMap.
-- | Combine the elements of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
10. Define foldMap in terms of foldr.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-}
