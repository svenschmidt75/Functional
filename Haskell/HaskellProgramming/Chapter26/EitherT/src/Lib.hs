{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib
    ( MyEitherT (..)
    ) where


data MyEitherT a m b = MyEitherT { runMyEitherT :: (m (Either a b)) }

instance Monad m => Functor (MyEitherT a m) where
--  fmap :: (a -> b) -> f             a -> f             b
    fmap :: (b -> c) -> MyEitherT a m b -> MyEitherT a m c
    fmap f (MyEitherT a) = MyEitherT $ (fmap . fmap) f a
