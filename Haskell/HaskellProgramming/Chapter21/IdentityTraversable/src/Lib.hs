module Lib
    ( Identity (..)
    ) where


data Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance Foldable Identity where
--foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f accum (Identity a) = f a accum

{-
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
   sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
      -- Defined in ‘Data.Traversable’
-}

instance Traversable Identity where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- fmap :: (a -> b) -> g a -> g b
-- with b => f b,
-- fmap :: (a -> f b) -> g a -> g (f b)
-- now flip the types of g(f b) around to f(g b)
    traverse f (Identity a) = Identity <$> f a
