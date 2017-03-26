module Main where

import Lib ( MaybeIO (..)
           , liftIO
           , liftMaybe)

{-
    Combining IO with Maybe.
    Because of the type signature of
    (>>=) :: Monad m => m a -> (a -> m b) -> m b,
    combining two monads like IO and Maybe is difficult.
    Here is code that will not work:
    main :: IO ()
    main = do
        resultIO <- runMaybeIO f
        resultMaybe <- resultIO
        print $ "Result: " ++ show resultMaube
    To see why, let us remove the syntactic sugar for <-:

    runMaybeIO f >>= \resultIO ->
        resultIO >>= \resultMaybe ->
            print $ "Result: " ++ show resultMaube

    Now, the type signature of the 1st >>= is
    (>>=) :: IO (Maybe Int) -> (Maybe Int -> IO (Maybe Int)) -> IO (Maybe Int)
    and that of the second is
    (>>=) ::Maybe Int -> (Int -> Maybe Int) -> Maybe Int
    To see the types involved, it is easier to do it inside-out.
    The inner function returns IO () (print), hence its type is

    :: Int -> IO ()
    \resultMaybe ->print $ "Result: " ++ show resultMaube

    but it should return Maybe Int.
    As we can see, this is impossible due to the IO, which we
    cannot get rid of. One solution is to combine Maybe with IO
    such that IO is outer-most.
    This is what MaybeIO does.
-}

f :: MaybeIO Int
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

main :: IO ()
main = do
    resultIO <- runMaybeIO f
    print $ "Result: " ++ show resultIO
