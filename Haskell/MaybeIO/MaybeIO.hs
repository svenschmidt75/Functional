module MaybeIO where

newtype MaybeIO a = MaybeIO { runMaybeIO :: IO (Maybe a) }

{- Defines two functions:
    1 data constructor: MaybeIO :: IO (Maybe a) -> MaybeIO a
    2.                  runMaybeIO :: MaybeIO a -> IO (Maybe a)
-}


instance Functor MaybeIO where
    fmap f = MaybeIO . fmap (fmap f) . runMaybeIO

instance Applicative MaybeIO where
    pure    = MaybeIO . pure . pure
    f <*> x = MaybeIO $ (<*>) <$> f' <*> x'
            where
                f' = runMaybeIO f
                x' = runMaybeIO x

instance Monad MaybeIO where
    return  = pure
    x >>= f = MaybeIO $ x' >>= runMaybeIO . mb . fmap f
            where
                x'          = runMaybeIO x
                mb :: Maybe (MaybeIO a) -> MaybeIO a
                mb (Just a) = a
                mb Nothing  = MaybeIO $ return Nothing

-- conversion functions
liftMaybe :: Maybe a -> MaybeIO a
liftMaybe x = MaybeIO (return x)

liftIO :: IO a -> MaybeIO a
liftIO x = MaybeIO (fmap Just x)



getInbox :: Maybe String
getInbox = Just "Inbox"

getFirstMail :: String -> Maybe String
getFirstMail folder = Just "1stMail"

getHeader :: String -> Maybe String
getHeader mail = Just "Mail Header"

f :: MaybeIO String
f = do
    i <- liftMaybe getInbox
    liftIO $ putStrLn "debug"
    m <- liftMaybe $ getFirstMail i
    h <- liftMaybe $ getHeader m
    return h


