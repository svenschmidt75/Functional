module Main where

import Prelude hiding (Word, getLine)
import qualified Prelude (Word, getLine)

import Test.QuickCheck


whitespace = ['\n', '\t', ' ']

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x whitespace = []
    | otherwise         = x : getWord xs

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x whitespace = (x:xs)
    | otherwise         = dropWord xs

dropSpace :: String -> String
dropSpace [] = []
dropSpace (x:xs)
    | elem x whitespace = dropSpace xs
    | otherwise         = (x:xs)


type Word = String

splitWords :: String -> [Word]
splitWords st = split (dropSpace st)

split :: String -> [Word]
split [] = []
split st = (getWord st) : splitWords (dropWord st)


type Line = [Word]

getLine :: Int -> [Word] -> Line
getLine _ [] = []
getLine len (w:ws)
    | length w <= len = w : restOfLine
    | otherwise       = []
      where
        newlen     = len - (length w + 1)
        restOfLine = getLine newlen ws

dropLine :: Int -> [Word] -> Line
dropLine _ [] = []
dropLine len (w:ws)
    | length w <= len = restOfLine
    | otherwise       = (w:ws)
      where
        newlen     = len - (length w + 1)
        restOfLine = dropLine newlen ws

lineLen = 20

splitLines :: [Word] -> [Line]
splitLines [] = []
splitLines ws = getLine lineLen ws : splitLines (dropLine lineLen ws)

fill :: String -> [Line]
fill = splitLines . splitWords

--joinLines :: [Line] -> String


main :: IO ()
main = do
    print $ getWord " boo"
    print $ getWord "cat dog"
