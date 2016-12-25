module Lib
    ( Three' (..)
    ) where

data Three' a b = Three' a b b
    deriving (Eq, Show)

instance Functor (Three' a) where
--  fmap :: (a -> b) -> f a -> f b
    fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

instance Foldable (Three' a) where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f accum (Three' _ b1 b2) = f b1 (f b2 accum)

instance Traversable (Three' a) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse g (Three' u a1 a2) = Three' u <$> g a1 <*> g a2
