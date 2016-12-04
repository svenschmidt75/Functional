module Lib
    ( j
    , l1
    , l2
    , a
    , meh
    , flipType
    ) where

-- join
j :: Monad m => m (m a) -> m a
-- (>>=) :: m a     -> (a   -> m b) -> m b
-- (>>=) :: m (m a) -> (m a -> m a) -> m a
j mma = mma >>= id

-- liftM
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = f <$> ma
--l1 f ma = ma >>= \a -> return $ f a

-- liftM2
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= \a' -> mb >>= \b -> return $ f a' b
{-
l2 f ma mb = do
    a <- ma
    b <- mb
    return $ f a b
-}

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = ma >>= \a' -> mf >>= \f -> return $ f a'

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh []     _ = return []
meh (x:xs) f = f x >>= \b -> (b:) <$> meh xs f
{-
meh (x:xs) f = do
    b <- f x
    (b:) <$> meh xs f
-}

flipType :: (Monad m) => [m a] -> m [a]
flipType [] = return []
flipType xs = meh xs id
