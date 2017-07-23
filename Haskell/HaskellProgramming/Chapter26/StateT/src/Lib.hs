{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( MyStateT (..)
    ) where

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

-- for checking the functor instance via checkers
instance Show (MyStateT Int Maybe String) where
    show :: a -> String
    show _ = ""

-- for checking the applicative instance via checkers
instance Show (MyStateT Int Maybe (String -> String)) where
    show :: a -> String
    show _ = ""

instance Eq (s -> m (a, s)) => Eq (MyStateT s m a) where
    (==) :: MyStateT s m a -> MyStateT s m a -> Bool
    (==) (MyStateT a) (MyStateT b) = a == b

instance Eq (Int -> Maybe (String, Int)) where
    (==) :: (Int -> Maybe (String, Int)) -> (Int -> Maybe (String, Int)) -> Bool
-- SS: This is nonsense, isn't it?
    (==) _ _ = True

instance Monad m => Functor (MyStateT s m) where
    fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
    fmap f (MyStateT smas) = MyStateT $ \s -> do as <- smas s
                                                 let (a, s') = as
                                                 return (f a, s')

instance Monad m => Applicative (MyStateT s m) where
--  pure :: a -> f            a
    pure :: a -> MyStateT s m a
    pure a = MyStateT $ \s -> pure (a, s)

--  (<*>) :: f            (a -> b) -> f a -> f b
    (<*>) :: MyStateT s m (a -> b) -> MyStateT s m a -> MyStateT s m b
    (<*>) (MyStateT smfab) (MyStateT sma) = MyStateT $ \s -> do mfab <- smfab s
                                                                let (fab, s') = mfab
                                                                ma <- sma s'
                                                                let (a, s'') = ma
                                                                return (fab a, s'')

instance Monad m => Monad (MyStateT s m) where
--  return :: a -> t            a
    return :: a -> MyStateT s m a
    return = pure

--  (>>=) :: t            a -> (a -> t            b) -> t            b
    (>>=) :: MyStateT s m a -> (a -> MyStateT s m b) -> MyStateT s m b
    (>>=) (MyStateT sma) f =  MyStateT $ \s -> do ma <- sma s
                                                  let (a, s') = ma
                                                  runMyStateT (f a) s'
