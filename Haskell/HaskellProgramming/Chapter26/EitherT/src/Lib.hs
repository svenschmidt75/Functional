{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( MyEitherT (..)
    ) where


newtype MyEitherT e m a = MyEitherT { runMyEitherT :: m (Either e a) }

{-
This code,
instance (Eq (m (Either e a))) => Eq (MyEitherT e m a) where

triggers error

    • The constraint ‘Eq (m (Either e a))’
        is no smaller than the instance head
      (Use UndecidableInstances to permit this)
    • In the instance declaration for ‘Eq (MyEitherT e m a)’


This is because teh compiler tries to make sure that
Eq (MyEitherT a m b) makes sense, which means that each
resolution step peels off one type constructor.
Here, the contraint is the definition of MyEitherT itself,
so it does not allow to remove one layer of constructor.
There are tricks to make this work, dor example by introducing
a newtype, see http://stackoverflow.com/questions/17863332/what-contraint-is-no-smaller-than-the-instance-head-means-and-how-to-solve-it
but here we just go with the UndecidableInstances extension...
-}
instance (Eq (m (Either e a))) => Eq (MyEitherT e m a) where
--  (==) :: a -> a -> Bool
    (==) (MyEitherT a) (MyEitherT b) = a == b

instance Show (MyEitherT e m a) where
--  show :: a -> String
    show _ = ""

instance Monad m => Functor (MyEitherT e m) where
--  fmap :: (a -> b) -> f             a -> f             b
    fmap :: (a -> b) -> MyEitherT e m a -> MyEitherT e m b
    fmap f (MyEitherT a) = MyEitherT $ (fmap . fmap) f a

instance Monad m => Applicative (MyEitherT e m) where
--  pure :: a -> f             a
    pure :: a -> MyEitherT e m a
    pure b = MyEitherT $ (pure . pure) b

--  (<*>) :: f (a -> b) -> f a -> f b
    (<*>) :: MyEitherT e m (a -> b) -> MyEitherT e m a -> MyEitherT e m b
    (<*>) (MyEitherT fab) (MyEitherT a) = MyEitherT $ (<*>) <$> fab <*> a

instance Monad m => Monad (MyEitherT e m) where
--  return :: a -> t             a
    return :: a -> MyEitherT e m a
    return = pure

--  (>>=) :: t             a -> (a -> t             b) -> t             b
    (>>=) :: MyEitherT e m a -> (a -> MyEitherT e m b) -> MyEitherT e m b
    (>>=) (MyEitherT a) f = MyEitherT $ do
                                v <- a
                                case v of
                                    Left u  -> return (Left u)
                                    Right u -> runMyEitherT (f u)
