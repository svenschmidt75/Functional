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

-- -- for checking the applicative instance via checkers
-- instance Show (MyReaderT Int Maybe (Int -> Int)) where
--     show :: a -> String
--     show _ = ""

instance Eq (MyStateT s m a) where
    (==) :: MyStateT s m a -> MyStateT s m a -> Bool
    (==) (MyStateT a) (MyStateT b) = True

-- instance Eq (Int -> Maybe Int) where
--     (==) :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> Bool
--     (==) _ _ = True

instance Monad m => Functor (MyStateT s m) where
    fmap :: (a -> b) -> MyStateT s m a -> MyStateT s m b
    fmap f (MyStateT smas) = MyStateT $ \s -> do
                                                  u <- smas s
                                                  let (x, y) = u
                                                  return (f x, y)

-- instance Monad m => Applicative (MyStateT r m) where
-- --  pure :: a -> f             a
--     pure :: a -> MyStateT r m a
--     pure a = MyStateT $ (pure . pure) a

-- --  (<*>) :: f (a -> b) -> f a -> f b
--     (<*>) :: MyStateT r m (a -> b) -> MyStateT r m a -> MyStateT r m b
--  We have to lift the apply function (<*>) over (->) r
--    in fab :: r -> m (a -> b).
--    The inner context is now m, and fa :: r -> m a, hence
--    ((<*>) <$> fab) <*> fa is what we want.
--    Note, the f in the right-most
--    (<*>) :: f (a -> b) -> f a -> f b
--    here is (->) r, not m!

--     Why does (<$>) (<*>) fab work?
--     The types:
--     (<$>) :: (a -> b) -> f a -> f b
--     (<*>) :: f (a -> b) -> f a -> f b

--     (<$>) (<*>)       fab
--        :: (a -> b) -> f a -> f b

--     hence (a -> b) ~ (<*>) :: h (u -> v) -> h u -> h v
--     with a ~ h (u -> v), b ~ h u -> h v
--     and f a ~ fab :: r -> m (x -> y), where f ~ (->) r, a ~ m (x -> y).

--     It follows that h ~ m, x ~ u, y ~ v, so and

--     f b :: r -> (m u -> m v)

--     and

--     ((<$>) (<*>) fab) <*> fb is well defined, where the <*> lifts
--     over the (->) r structure.

--     (<*>) (MyReaderT fab) (MyReaderT fa) = MyReaderT $ ((<*>) <$> fab) <*> fa
-- --    (<*>) (MyReaderT fab) (MyReaderT fa) = MyReaderT $ \r -> (fab r) <*> (fa r)

-- instance Monad m => Monad (MyReaderT r m) where
-- --  return :: a -> t             a
--     return :: a -> MyReaderT e m a
--     return = pure

-- --  (>>=) :: t             a -> (a -> t             b) -> t             b
--     (>>=) :: MyReaderT r m a -> (a -> MyReaderT r m b) -> MyReaderT r m b
--     (>>=) (MyReaderT a) f =  MyReaderT $ \r -> do
--                                                 a1 <- a r
--                                                 runMyReaderT (f a1) r
