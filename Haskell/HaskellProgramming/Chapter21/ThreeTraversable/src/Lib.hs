module Lib
    ( Three (..)
    ) where

data Three a b c = Three a b c
    deriving (Eq, Show)

instance Functor (Three a b) where
--  fmap :: (a -> b) -> f a -> f b
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
--  foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f accum (Three _ _ c) = f c accum

instance Traversable (Three a b) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse g (Three u v a) = Three u v <$> g a

{- g a :: f b, but the result is f (t b).
 - So, use <$> to hoist t into f b...
 -}
