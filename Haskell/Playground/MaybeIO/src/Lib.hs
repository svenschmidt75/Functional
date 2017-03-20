{-# LANGUAGE InstanceSigs #-}
module Lib
    ( MaybeIO (..)
    , liftIO
    ) where


newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

-- instance Eq (MaybeIO a) where
--     (==) :: MaybeIO a -> MaybeIO a -> Bool
--     (==) (MaybeIO a) (MaybeIO b) = do
--         a1 <- a
--         b1 <- b
--         return (a1 == b1)

instance Show (MaybeIO a) where
    show :: MaybeIO a -> String
    show _ = "MaybeIO a"

instance Functor MaybeIO where
    fmap :: (a -> b) -> MaybeIO a -> MaybeIO b
    fmap f (MaybeIO ma) = MaybeIO $ (fmap . fmap) f ma

instance Applicative MaybeIO where
    pure :: a -> MaybeIO a
    pure a = MaybeIO $ (return . Just) a

    (<*>) :: MaybeIO (a -> b) -> MaybeIO a -> MaybeIO b
    (<*>) (MaybeIO mf) (MaybeIO ma) = MaybeIO $ do
        maybeF <- mf
        maybeA <- ma
        return (maybeF <*> maybeA)

instance Monad MaybeIO where
    return :: a -> MaybeIO a
    return = pure

    (>>=) :: MaybeIO a -> (a -> MaybeIO b) -> MaybeIO b
    (>>=) (MaybeIO ioma) f = MaybeIO $ ioma >>= \ma -> case ma of
        Just a  -> runMaybeIO (f a)
        Nothing -> return Nothing


-- (>>=) :: IO a -> (a -> IO b) -> IO b, b :: Maybe a
liftIO :: IO a -> MaybeIO a
--liftIO ioa = MaybeIO $ ioa >>= \a -> return $ Just a
--liftIO ioa = MaybeIO $ ioa >>= return . Just
liftIO ioa = MaybeIO $ Just <$> ioa
