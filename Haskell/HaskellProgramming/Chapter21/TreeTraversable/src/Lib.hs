module Lib
    ( Tree (..)
    ) where

data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
--  fmap :: (a -> b) -> f     a -> f     b
    fmap = undefined

-- foldMap is a bit easier and looks more natural,
-- but you can do foldr too for extra credit.
instance Foldable Tree where
--  foldr :: (a -> b -> b) -> b -> t     a -> b
    foldMap = undefined

instance Traversable Tree where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse = undefined
