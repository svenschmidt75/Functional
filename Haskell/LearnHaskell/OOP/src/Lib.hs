module Lib where
--    ( Identity (..)
--    ) where

-- make a cup object
cup' :: Int -> Int
cup' flOz = flOz

-- get fluid ounces in cup
getOz' :: Int -> Int
getOz' flOz = flOz

drink' aCup ozDrank = cup' (flOz - ozDrank)
    where flOz = getOz' aCup


-- alternative with functions

type Cup = (Int -> Int) -> Int

-- cup constructor
-- feels like an additional level of indirection...
-- How is this benefitial compared to just an Int?
mkCup :: Int -> Cup
mkCup flOz = \message -> message flOz

getOz :: Cup -> Int
getOz cup = cup id

drink :: Cup -> Int -> Cup
drink aCup ozDrank = mkCup (flOz - ozDrank)
    where flOz = getOz aCup


-- How is this related to the Reader monad?
