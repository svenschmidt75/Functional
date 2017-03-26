module Main where

import Lib ( MaybeIO (..)
           , liftIO
           , liftMaybe)


-- f :: IO (Maybe Int)
-- f = do
--     putStrLn "Enter a number: "
--     a <- read <$> getLine :: IO Int
--     putStrLn ("The number you entered is " ++ show a)
--     let mb = if a < 10 then
--                  Just a
--              else
--                  Nothing
--     mb >>= \value -> do
--         putStrLn ("Number: " ++ show value)
--         return value

{-
    a <- read <$> getLine :: IO Int

    read <$> getLine >>= \a -> return b

    where:

    (>>=) :: IO Int -> (Int -> IO b) -> IO b

    want:
        (>>=) :: MaybeIO Int -> (Int -> MaybeIO b) -> MaybeIO b

    Need to convert: IO a -> MaybeIO a...
-}

f :: MaybeIO Int
f = do
        liftIO $ putStrLn "Enter a number: "
        a <- liftIO $ (read <$> getLine :: IO Int)
        return a
        -- liftIO $ putStrLn ("The number you entered is " ++ show a)
        -- let mb = if a < 10 then
        --              Just a
        --          else
        --              Nothing
        -- liftIO $ putStrLn ("Number: " ++ show mb)
        -- return mb

main :: IO ()
main = do
    result <- runMaybeIO f
    print $ "Result: " ++ show result
