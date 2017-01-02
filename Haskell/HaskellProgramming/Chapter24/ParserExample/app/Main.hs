module Main where

import Text.Trifecta

import Lib


pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse (stop :: Parser Int)
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
    pNL "parseStr: 1"
    print $ parseString parseStr mempty "1"
    pNL "parseStr: 12"
    print $ parseString parseStr mempty "12"
    pNL "parseStr: 123"
    print $ parseString parseStr mempty "123"
    pNL "parseStr': 1"
    print $ parseString parseStr' mempty "1"
    pNL "parseStr': 12"
    print $ parseString parseStr' mempty "12"
    pNL "parseStr': 123"
    print $ parseString parseStr' mempty "123"
