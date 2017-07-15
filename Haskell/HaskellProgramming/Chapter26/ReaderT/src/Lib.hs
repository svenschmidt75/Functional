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

newtype MyReader r a = MyReader { runMyReader :: r -> a }

The difference is that the result, a, is changed such that
the reader returns a type with a monadic structure, i.e. m a.
-}
newtype MyReaderT r m a = MyReaderT { runMyReaderT :: r -> m a }

-- for checking the functor instance via checkers
instance Show (MyReaderT Int Maybe Int) where
    show :: a -> String
    show _ = ""

-- for checking the applicative instance via checkers
instance Show (MyReaderT Int Maybe (Int -> Int)) where
    show :: a -> String
    show _ = ""

instance Eq (r -> m a) => Eq (MyReaderT r m a) where
    (==) :: MyReaderT r m a -> MyReaderT r m a -> Bool
    (==) (MyReaderT a) (MyReaderT b) = a == b

instance Eq (Int -> Maybe Int) where
    (==) :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> Bool
    (==) _ _ = True

instance Monad m => Functor (MyReaderT r m) where
    fmap :: (a -> b) -> MyReaderT r m a -> MyReaderT r m b
    fmap f (MyReaderT a) = MyReaderT $ \r -> f <$> a r

instance Monad m => Applicative (MyReaderT r m) where
--  pure :: a -> f             a
    pure :: a -> MyReaderT r m a
    pure a = MyReaderT $ (pure . pure) a

--  (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: MyReaderT r m (a -> b) -> MyReaderT r m a -> MyReaderT r m b
-- Use <$> to lift <*> over m
    (<*>) (MyReaderT fab) (MyReaderT fa) = MyReaderT $ ((<*>) <$> fab) <*> fa
--    (<*>) (MyReaderT fab) (MyReaderT fa) = MyReaderT $ \r -> (fab r) <*> (fa r)

