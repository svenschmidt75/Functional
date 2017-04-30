module Main where

import Lib ( MyMaybeT (..)
           , liftIO
           , liftMaybe)

f :: MyMaybeT IO Int
f = do
        liftIO $ putStrLn "Enter a number: "
        a <- liftIO $ read <$> getLine
        liftIO $ putStrLn ("The number you entered is " ++ show a)
        let mb = if a < 10 then
                     Just a
                 else
                     Nothing
        liftIO $ putStrLn ("Number: " ++ show mb)
        liftMaybe mb

g :: MyMaybeT IO String
g = do
    i <- liftMaybe getInbox
    liftIO $ putStrLn "debug"
    m <- liftMaybe $ getFirstMail i
    h <- liftMaybe $ getHeader m
    return h

getInbox :: Maybe String
getInbox = Just "Inbox"

getFirstMail :: String -> Maybe String
getFirstMail folder = Just "1stMail"

getHeader :: String -> Maybe String
getHeader mail = Just "Mail Header"

main :: IO ()
main = do
    resultIO <- runMyMaybeT f
    print $ "Result: " ++ show resultIO
    resultIO2 <- runMyMaybeT g
    print $ show resultIO2
