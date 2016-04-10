module Exercise415 where

import Test.QuickCheck hiding (Result)

data Seasons = Spring
             | Summer
             | Autumn
             | Winter
             deriving (Show, Eq, Ord)

data Temp = Cold
          | Hot
          deriving (Eq, Show, Ord)

seasonToTemp :: Seasons -> Temp
seasonToTemp s
    | s > Summer = Cold
    | otherwise  = Hot

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
        deriving (Show, Eq, Ord)

monthToSeason :: Month -> Seasons
monthToSeason m
    | m < April    = Winter
    | m < July     = Spring
    | m < October  = Summer
    | m < December = Autumn
    | otherwise    = Winter

main :: IO ()
main = do
        print (seasonToTemp Spring)
        print (seasonToTemp Winter)
        print (monthToSeason January)
        print (monthToSeason October)
