module Dependence where

-- The way to deal with state is to pass that state into
-- the lambdas that the functions return. Similar to state
-- monad.

-- pass the state, here 4, in
main = putStrLn (f "foo" 4)
        where f = left `composePref` right

-- now left returns a function that takes the config parameter
-- i, or the state
left :: String -> Pref String
left s = \i -> (repeatString i "< ") ++ s

right :: String -> Pref String
right s = \i -> s ++ (repeatString i " >")

repeatString :: Integer -> String -> String
repeatString count str = if count <= 0
                            then ""
                            else str ++ (repeatString (count - 1) str)

type Config = Integer

-- accept a value of type a and return a function that takes a Config value
-- and returns that a (similar to State Monad)
type Pref a = (Config -> a)


composePref :: (String -> Pref String) -> (String -> Pref String) -> (String -> Pref String)
composePref f g s = \c -> let y = g s c
                            in f y c
