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
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- traverse g (Identity a) => g a :: f b, but we need to return
-- type 'f (Identity b)'. This means we need to move the data
-- constructor 'Identity' past context f without modifying it.
-- This is what fmap does!!! We are lifting 'Identity' into the
-- context f.
-- NOTE: Do NOT define traverse in terms of sequence!!!
-- This will cause an infinite loop, or bottom?
-- i.e. that call will not terminate
    traverse f (Identity a) = Identity <$> f a
