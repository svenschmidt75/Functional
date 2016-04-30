module Picture where

import Data.List


type Picture = [[Char]]

flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
--flipV = map reverse
flipV pic = [reverse line | line <- pic]

above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
beside left right = [leftLine ++ rightLine | (leftLine, rightLine) <- zip left right]





main :: IO ()
main = do
    putStrLn "Stuff"
