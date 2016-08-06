module Lib
    ( someFunc
    , DaPhone (..)
    , cellPhonesDead
    , fingerTaps
    ) where


import Data.Char
import qualified Data.Map as Map


-- fill in the rest.
data DaPhone = DaPhone

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

encoderMap :: Map.Map Char (Digit, Presses)
encoderMap = Map.fromList [
             ('1', ('1', 1)),
             ('a', ('2', 1)), ('b', ('2', 2)), ('c', ('2', 3)), ('2', ('2', 4)),
             ('d', ('3', 1)), ('e', ('3', 2)), ('f', ('3', 3)), ('3', ('3', 4)),
             ('g', ('4', 1)), ('h', ('4', 2)), ('i', ('4', 3)), ('4', ('4', 4)),
             ('j', ('5', 1)), ('k', ('5', 2)), ('l', ('5', 3)), ('5', ('5', 4)),
             ('m', ('6', 1)), ('n', ('6', 2)), ('o', ('6', 3)), ('6', ('6', 4)),
             ('p', ('7', 1)), ('q', ('7', 2)), ('r', ('7', 3)), ('s', ('7', 4)), ('7', ('7', 5)),
             ('t', ('8', 1)), ('u', ('8', 2)), ('v', ('8', 3)), ('8', ('8', 4)),
             ('w', ('9', 1)), ('x', ('9', 2)), ('y', ('9', 3)), ('z', ('9', 4)), ('9', ('9', 5)),
             ('^', ('*', 1)),
             ('+', ('0', 1)), (' ', ('0', 2)),
             ('.', ('#', 1)), (',', ('#', 2))]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps _ c = convertToCapital ++ convertChar
                  where
                      convertToCapital = if c >= 'A' && c <= 'Z'
                                           then [('*', 1)]
                                         else
                                           []
                      convertChar = case (Map.lookup (toLower c) encoderMap) of
                                      Just a -> [a]
                                      Nothing -> error [c]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead _ [] = []
cellPhonesDead p (x:xs) = reverseTaps p x ++ cellPhonesDead p xs

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps [] = 0
fingerTaps ((_, i):xs) = i + fingerTaps xs

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined



someFunc :: IO ()
someFunc = putStrLn "someFunc"