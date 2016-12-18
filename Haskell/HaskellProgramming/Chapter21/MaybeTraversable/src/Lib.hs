module Lib
    ( Optional (..)
    ) where

data Optional a = Nada
                | Yep a
    deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada    = Nada
    fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
-- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr _ accum Nada = accum
    foldr f accum (Yep a) = f a accum

instance Traversable Optional where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a
