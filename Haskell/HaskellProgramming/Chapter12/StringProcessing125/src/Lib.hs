module Lib
    ( notThe
    , replaceThe
    , replaceTheRecursive
    , countTheBeforeVowel
    , countVowels
    ) where

notThe :: String -> Maybe String
notThe = undefined

replaceThe :: String -> String
replaceThe input = unwords $ map replace $ words input
                   where
                     replace "the" = "a"
                     replace w     = w

replaceTheRecursive :: String -> String
replaceTheRecursive input = let w = words input
                            in unwords $ replace w
                            where
                                replace :: [String] -> [String]
                                replace []         = []
                                replace ("the":xs) = ['a'] : replace xs
                                replace (x:xs)     = x : replace xs

isVowel :: Char -> Bool
isVowel 'a' = True
isVowel 'e' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel 'u' = True
isVowel 'A' = True
isVowel 'E' = True
isVowel 'I' = True
isVowel 'O' = True
isVowel 'U' = True
isVowel _   = False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel input = let wrds = words input
                            in count' wrds
                            where
                              count' (x:y:xs) = let cnt = case x of
                                                            "the" -> let (v:_) = y
                                                                     in if isVowel v
                                                                          then 1
                                                                          else 0
                                                            _     -> 0
                                                in cnt + count' (y:xs)
                              count' _        = 0

vowelsInString :: String -> [Char]
vowelsInString = filter isVowel

countVowels :: String -> Integer
countVowels input = let wrds = words input
                    in foldr (\vowels accum -> accum + toInteger (length . vowelsInString $ vowels)) 0 wrds
