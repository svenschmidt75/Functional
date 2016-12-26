module Lib
    ( Tree (..)
    ) where


data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
--  fmap :: (a -> b) -> f     a -> f     b
    fmap _ Empty        = Empty
    fmap f (Leaf a)     = Leaf $ f a
    fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

unfold' :: Monoid m => Tree m -> m
unfold' Empty        = mempty
unfold' (Leaf a)     = a
unfold' (Node l a r) = unfold' l `mappend` a `mappend` unfold' r

instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> t     a -> b
--  foldMap :: Monoid m => (a -> m) -> t    a -> m
--  foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f t = unfold' $ f <$> t
-- or
--    foldMap _ Empty        = mempty
--    foldMap f (Leaf a)     = f a
--    foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

instance Traversable Tree where
-- traverse :: Applicative f => (a -> f b) -> t    a -> f (t    b)
-- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)

-- For traverse, think Functor but with some Functor thrown in.
    traverse _ Empty        = pure Empty
    traverse f (Leaf a)     = Leaf <$> f a
    traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r
-- or
    -- traverse f t = unfoldTraverse $ f <$> t :: Tree (f b)
    -- unfoldTraverse :: Applicative f => Tree (f b) -> f (Tree b) - or sequenceA
