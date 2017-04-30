{-# LANGUAGE InstanceSigs #-}
module Lib
    ( MyMaybeT (..)
    ) where

newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance Functor m => Functor (MyMaybeT m) where
--  fmap :: (a -> b) -> f a -> f b
    fmap :: (a -> b) -> MyMaybeT m a -> MyMaybeT m b
    fmap f (MyMaybeT ma) = MyMaybeT $ (fmap . fmap) f ma
