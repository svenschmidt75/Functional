module Lib
    ( State (..)
    , get
    , put
    , exec
    , eval
    , modify
    , state
    ) where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
-- fmap :: (a -> b) -> f       a -> f       b
-- fmap :: (a -> b) -> State s a -> State s b
   fmap f (State sas) = State $ \s -> let (a, s') = sas s
                                      in (f a, s')

instance Applicative (State s) where
-- pure :: a -> f       a
-- pure :: a -> State s a
    pure a = State $ \s -> (a, s)

-- (<*>) :: f       (a -> b) -> f       a -> f       b
-- (<*>) :: State s (a -> b) -> State s a -> State s b
    (<*>) (State sab) (State sa) = State $ \s -> let (a, s') = sa s
                                                     (f, s'') = sab s'
                                                 in (f a, s'')

instance Monad (State s) where
    return = pure

-- (>>=) :: m       a -> (a -> m       b) -> m       b
-- (>>=) :: State s a -> (a -> State s b) -> State s b
    (>>=) (State sa) f = State $ \s -> let (a, s') = sa s
                                           applied = runState $ f a
                                       in applied s'

-- from Control.Monad.State
-- get :: m s Source
-- Return the state from the internals of the monad.
get :: State s s
get = State $ \s -> (s, s)

-- put :: s -> m () Source
-- Replace the state inside the monad.
put :: s -> State s ()
put s = State $ \_ -> ((), s)

exec :: State s a -> s -> s
exec (State sa) s = let (_, s') = sa s
                    in s'

eval :: State s a -> s -> a
eval (State sa) = \s -> let (a, _) = sa s
                        in a

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

state :: (s -> (a, s)) -> State s a
state f = State $ \s -> f s
