module Lib
    ( State (..)
    ) where

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
-- fmap :: (a -> b) -> f       a -> f       b
-- fmap :: (a -> b) -> State s a -> State s b
   fmap f (State ssa) = State $ \s -> let (s', a) = ssa s
                                      in (s', f a)

instance Applicative (State s) where
-- pure :: a -> f       a
-- pure :: a -> State s a
    pure a = State $ \s -> (s, a)

-- (<*>) :: f       (a -> b) -> f       a -> f       b
-- (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State sab) (State sa) = State $ \s -> let (s', a) = sa s
                                                     (s'', f) = sab s'
                                                 in (s'', f a)

instance Monad (State s) where
    return = pure

-- (>>=) :: m       a -> (a -> m       b) -> m       b
-- (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State sa) f = State $ \s -> let (s', a) = sa s
                                           applied = runState $ f a
                                       in applied s'

mkState :: (s -> (s, a)) -> State s a
mkState = State
