{-# LANGUAGE InstanceSigs #-}
module Lib
    ( Compose (..)
    ) where

import Control.Applicative (liftA2)


newtype Compose f g a = Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
--  fmap :: (a -> b) -> f           a -> f           b
    fmap :: (a -> b) -> Compose f g a -> Compose f g b
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose (pure (pure a))

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ liftA2 (<*>) f a

instance (Functor f, Foldable f, Foldable g) => Foldable (Compose f g) where
--  foldMap :: Monoid m => (a -> m) -> t           a -> m
    foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
    foldMap f (Compose fga) = foldMap id (foldMap f <$> fga)

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
--  traverse :: Applicative h => (a -> h b) -> t           a -> h (t           b)
    traverse :: Applicative h => (a -> h b) -> Compose f g a -> h (Compose f g b)
--  traverse h <$> fga :: f (h (g b))
--  sequenceA :: Applicative f -> t (f a) -> f (t a)
--  sequenceA $ traverse h <$> fga :: h (f (g b))
    traverse h (Compose fga) = Compose <$> (sequenceA $ traverse h <$> fga)
