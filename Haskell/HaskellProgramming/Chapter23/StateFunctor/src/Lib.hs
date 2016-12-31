module Lib
    ( State (..)
    ) where

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
-- fmap :: (a -> b) -> f       a -> f       b
-- fmap :: (a -> b) -> State s a -> State s b
   fmap f (State ssa) = State $ \s -> let (s', a) = ssa s
                                      in (s', f a)
