{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
    ( MyEitherT (..)
    ) where


newtype MyEitherT a m b = MyEitherT { runMyEitherT :: m (Either a b) }

{-
This code,
instance (Eq (m (Either a b))) => Eq (MyEitherT a m b) where

triggers error

    • The constraint ‘Eq (m (Either a b))’
        is no smaller than the instance head
      (Use UndecidableInstances to permit this)
    • In the instance declaration for ‘Eq (MyEitherT a m b)’


This is because teh compiler tries to make sure that
Eq (MyEitherT a m b) makes sense, which means that each
resolution step peels off one type constructor.
Here, the contraint is the definition of MyEitherT itself,
so it does not allow to remove one layer of constructor.
There are tricks to make this work, dor example by introducing
a newtype, see http://stackoverflow.com/questions/17863332/what-contraint-is-no-smaller-than-the-instance-head-means-and-how-to-solve-it
but here we just go with the UndecidableInstances extension...
-}
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

instance Monad m => Applicative (MyEitherT a m) where
--  pure :: b -> f             b
    pure :: b -> MyEitherT a m b
    pure b = MyEitherT $ (pure . pure) b

--  (<*>) :: f (b -> c) -> f b -> f c
    (<*>) :: MyEitherT a m (b -> c) -> MyEitherT a m b -> MyEitherT a m c
    (<*>) (MyEitherT fbc) (MyEitherT b) = MyEitherT $ (<*>) <$> fbc <*> b

instance Monad m => Monad (MyEitherT a m) where
--  return :: b -> t             b
    return :: b -> MyEitherT a m b
    return = pure

--  (>>=) :: t             b -> (b -> t             c) -> t             c
    (>>=) :: MyEitherT a m b -> (b -> MyEitherT a m c) -> MyEitherT a m c
    (>>=) (MyEitherT b) f = MyEitherT $ do
                                v <- b
                                case v of
                                    Left a  -> return (Left a)
                                    Right c -> runMyEitherT (f c)
