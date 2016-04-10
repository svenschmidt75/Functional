module Exercise411 where

import Test.QuickCheck hiding (Result)


data Move = Rock
          | Paper
          | Scissors
          deriving (Show, Eq)

beat :: Move -> Move
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

lose :: Move -> Move
lose Rock  = Scissors
lose Paper = Rock
lose _     = Paper

data Result = Win
            | Lose
            | Draw
            deriving (Show)

outcome :: Move -> Move -> Result
outcome Rock     Scissors = Win
outcome Scissors Paper    = Win
outcome Paper    Rock     = Win
outcome x y
    | x == y    = Draw
    | otherwise = Lose

main :: IO ()
main = do
        print (outcome Rock Scissors)
