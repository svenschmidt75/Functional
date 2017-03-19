module Main where

import Lib ( MaybeIO (..)
           , liftIO)


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

-- f :: MaybeIO Int
-- f = MaybeIO $ do
--        liftIO $ putStrLn "Enter a number: "
--        (Just <$> (read <$> getLine :: IO Int)) :: IO (Maybe Int)
--    MaybeIO (return $ Just a)
    -- putStrLn ("The number you entered is " ++ show a)
    -- let mb = if a < 10 then
    --              Just a
    --          else
    --              Nothing
    -- value <- value
    -- putStrLn ("Number: " ++ show value)
    -- return value


main :: IO ()
main = do
--    result <- runMaybeIO f
    print "result"
