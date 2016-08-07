module Main where

import Lib

convo :: [String]
convo = [
        "Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"
        ]

main :: IO ()
main = do
    print $ map mostPopularLetter convo
    print $ coolestLtr convo
