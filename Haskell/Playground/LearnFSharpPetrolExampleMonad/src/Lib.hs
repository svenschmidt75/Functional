module Lib where

import Control.Monad.State


drive :: Fractional a => String -> a -> a
drive distance petrol
    | distance == "far"    = petrol / 2
    | distance == "medium" = petrol - 10
    | otherwise            = petrol - 1

withoutState :: Fractional a => a -> a
withoutState initialPetrol =
    let s1 = drive "far" initialPetrol
        s2 = drive "medium" s1
        s3 = drive "short" s2
    in s3

driveState :: Fractional a => String -> State a ()
driveState distance
    | distance == "far"    = state $ \petrol -> ((), petrol / 2)
    | distance == "medium" = state $ \petrol -> ((), petrol - 10)
    | otherwise            = state $ \petrol -> ((), petrol - 1)

withStateM :: Fractional a => State a ()
withStateM = do
    driveState "far"
    driveState "medium"
    driveState "short"
    return ()

withStateM' :: Fractional a => State a ()
withStateM' = mapM_ driveState ["far", "medium", "short"]

someFunc :: IO ()
someFunc = do
    print $ withoutState 100
    print $ execState withStateM 100
    print $ execState withStateM' 100
