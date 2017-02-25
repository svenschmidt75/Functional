module Main where

import Lib


foo1 :: Reader String String
foo1 = Reader $ \r -> "Hello, " ++ r

foo2 :: Reader String String
foo2 = Reader $ \r -> "Hello, " ++ r

foo3 :: Reader String String
foo3 = do
    r1 <- foo1
    r2 <- foo2
    return $ r1 ++ r2

foo3' :: Reader String String
foo3' = foo1 >>= \r1 ->
          foo2 >>= \r2 ->
            return $ r1 ++ r2

main :: IO ()
main = do
    print $ runReader foo3 "Michelle"
    print $ runReader foo3' "Michelle"
