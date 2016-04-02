module Desctruction where

-- 1. print message to screen
-- 2. wait for user input
waitForInput :: String -> IOAction String
waitForInput = \s -> Output s (Wait (\t -> Return t))

outputReverse :: String -> IOAction ()
outputReverse = \s -> Output (reverse s) (Return ())

main = runIO ((outputReverse `composeIO` waitForInput) "Enter some text:")

data IOAction a = Output String (IOAction a)  -- action of what to do next
                | Wait (String -> IOAction a) -- a continuation that determines what to do next
                | Return a

-- interpreter for these actions
runIO :: IOAction a -> IO a
runIO (Output s n) = do { putStrLn s ; runIO n }
runIO (Wait f) = do { s <- getLine ; runIO (f s) }
runIO (Return a) = do { return a }

unitIO :: a -> IOAction a
unitIO a = Return a

-- composition
composeIO :: (b -> IOAction c) -> (a -> IOAction b) -> (a -> IOAction c)
composeIO f g x = case (g x) of
                            Output s n -> Output s (composeIO f (\_ -> n) ())
                            Wait h     -> Wait (\s -> composeIO f h s)
                            Return a   -> f a

-- identity
idIO :: a -> IOAction a
idIO x = Return x
