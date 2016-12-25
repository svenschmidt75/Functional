module Lib
    ( S (..)
    ) where

data S n a = S (n a) a
    deriving (Eq, Show)

instance Functor n => Functor (S n) where
--  fmap :: (a -> b) -> f     a -> f     b
--  fmap :: (a -> b) -> (S n) a -> (S n) b
    fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
--  foldr :: (a -> b -> b) -> b -> t     a -> b
--  foldr :: (a -> b -> b) -> b -> (S n) a -> b
    foldr f accum (S na a) = foldr f (f a accum) na

instance Traversable n => Traversable (S n) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f (S na a) = S <$> (traverse f na) <*> f a
