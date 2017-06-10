{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( MyReaderT (..)
    ) where

{-
The reader transformer monad add additional structure to the
result, compared to the reader monad itself, (->) r.
-}
newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }

instance Show (MyReaderT Int Maybe Int) where
    show :: a -> String
    show _ = ""

instance Eq (r -> m a) => Eq (MyReaderT r m a) where
    (==) :: MyReaderT r m a -> MyReaderT r m a -> Bool
    (==) (MyReaderT a) (MyReaderT b) = a == b

instance Monad m => Functor (MyReaderT r m) where
    fmap :: (a -> b) -> MyReaderT r m a -> MyReaderT r m b
    fmap f (MyReaderT a) = MyReaderT $ \r -> f <$> a r
