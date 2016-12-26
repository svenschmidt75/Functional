module Lib
    ( Tree (..)
    ) where

import Data.Semigroup


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
--  fmap :: (a -> b) -> f     a -> f     b
    fmap _ Empty        = Empty
    fmap f (Leaf a)     = Leaf $ f a
    fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.
instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> t     a -> b
--  foldMap :: Monoid m => (a -> m) -> t    a -> m
--  foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Empty        = mempty
    foldMap f (Leaf a)     = f a
    foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r
--    foldMap f a =

instance Traversable Tree where
-- traverse :: Applicative f => (a -> f b) -> t    a -> f (t    b)
-- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse f Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node l a r) = Node <$> traverse f l <*> (f a) <*> (traverse f r)
