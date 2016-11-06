module Lib
    ( W (..)
    , g
    , h
    , join
    ) where

data W a = W a
    deriving (Show, Eq)

instance Functor W where
    fmap f (W a) = W $ f a

instance Applicative W where
    pure a = W a
    -- (<*>) :: f (a -> b) -> f a -> f b
    --          W (a -> b) -> W a -> W b
    (<*>) (W f) (W a) = W $ f a

instance Monad W where
    return = pure
    -- (>>=) :: m a -> (a -> m b) -> m b
    --          W a -> (a -> W b) -> W b
    (W a) >>= f = f a

g :: Int -> W Int -> W Int
--g x (W y) = W (x+y)
g x mx = mx >>= (\v -> W $ x + v)

h :: W Int -> W Int -> W Int
--h (W x) (W y) = W (x+y)
h ma mb = do
    a <- ma
    b <- mb
    return $ a + b

join :: W (W a) -> W a
join mma = mma >>= id
