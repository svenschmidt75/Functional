{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( MyEitherT (..)
    ) where


newtype MyEitherT a m b = MyEitherT { runMyEitherT :: m (Either a b) }

instance (Eq (m (Either a b))) => Eq (MyEitherT a m b) where
--  (==) :: a -> a -> Bool
    (==) (MyEitherT a) (MyEitherT b) = a == b

instance Show (MyEitherT a m b) where
--  show :: a -> String
    show _ = ""

instance Monad m => Functor (MyEitherT a m) where
--  fmap :: (a -> b) -> f             a -> f             b
    fmap :: (b -> c) -> MyEitherT a m b -> MyEitherT a m c
    fmap f (MyEitherT a) = MyEitherT $ (fmap . fmap) f a
