module Lib
    ( MaybeIO (..)
    , liftIO
    ) where


newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }


-- (>>=) :: IO a -> (a -> IO b) -> IO b, b :: Maybe a
liftIO :: IO a -> MaybeIO a
--liftIO ioa = MaybeIO $ ioa >>= \a -> return $ Just a
--liftIO ioa = MaybeIO $ ioa >>= return . Just
liftIO ioa = MaybeIO $ Just <$> ioa
