{-# LANGUAGE InstanceSigs #-}
module Lib
    ( MyMaybeT (..)
    , liftMaybe
    , liftIO
    ) where

import Control.Applicative (liftA2)


newtype MyMaybeT m a = MyMaybeT { runMyMaybeT :: m (Maybe a) }

instance Functor m => Functor (MyMaybeT m) where
--  fmap :: (a -> b) -> f          a -> f          b
    fmap :: (a -> b) -> MyMaybeT m a -> MyMaybeT m b
    fmap f (MyMaybeT ma) = MyMaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MyMaybeT m) where
--  pure :: a -> MyMaybe m a
    pure :: a -> MyMaybeT m a
    pure a = MyMaybeT $ (pure . pure) a

--  (<*>) :: f          (a -> b) -> f          a -> f          b
    (<*>) :: MyMaybeT m (a -> b) -> MyMaybeT m a -> MyMaybeT m b
    (<*>) (MyMaybeT mf) (MyMaybeT ma) = MyMaybeT $ liftA2 (<*>) mf ma

instance Monad m => Monad (MyMaybeT m) where
--  return :: a -> m a
    return :: a -> MyMaybeT m a
    return = pure

--  (>>=) ::          m a -> (a ->          m b) ->          m b
    (>>=) :: MyMaybeT m a -> (a -> MyMaybeT m b) -> MyMaybeT m b
    (>>=) (MyMaybeT ma) f = MyMaybeT $ ma >>= \b -> case b of
                                                        Just a  -> runMyMaybeT (f a)
                                                        Nothing -> return Nothing

liftMaybe :: Monad m => Maybe a -> MyMaybeT m a
liftMaybe a = MyMaybeT (return a)

liftIO :: IO a -> MyMaybeT IO a
liftIO ioa = MyMaybeT $ Just <$> ioa
