module Lib
    ( Constant (..)
    ) where

newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ accum (Constant _) = accum

{-
class (Functor t, Foldable t) => Traversable (t :: * -> *) where
   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
       mapM :: Monad m => (a -> m b) -> t a -> m (t b)
   sequence :: Monad m => t (m a) -> m (t a)
  {-# MINIMAL traverse | sequenceA #-}
      -- Defined in ‘Data.Traversable’
-}

instance Traversable (Constant a) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse _ (Constant a) = Constant <$> pure a
