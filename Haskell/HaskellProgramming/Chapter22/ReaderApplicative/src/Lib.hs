{-# LANGUAGE InstanceSigs #-}
module Lib where

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
-- fmap :: (a -> b) -> f      a -> f      b
-- fmap :: (a -> b) -> (->) r a -> (->) r b
    fmap f fa = Reader $ f . f'
                where
                    f' = runReader fa

instance Applicative (Reader r) where
-- pure :: a -> f      a
-- pure :: a -> (->) r a
    pure :: a -> Reader r a
--    pure a = Reader $ \_ -> a
    pure a = Reader $ const a

-- (<*>) :: f      (a -> b) -> f      a -> f      b
-- (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)
