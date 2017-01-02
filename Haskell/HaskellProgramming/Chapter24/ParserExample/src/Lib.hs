module Lib where

import Control.Applicative
import Text.Trifecta


stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one :: Parser ()
one = char '1' >> eof

-- read a single character '1', then die
-- equivalent to char '1' >> stop
one' :: Parser Int
one' = one >> (stop :: Parser Int)

-- read two characters, '1', and '2'
oneTwo :: Parser ()
oneTwo = char '1' >> char '2' >> eof

-- read two characters, '1' and '2', then die
oneTwo' :: Parser Int
oneTwo' = oneTwo >> (stop :: Parser Int)

testParse :: Show a => Parser a -> IO ()
testParse p = print $ parseString p mempty "123"

parseStr :: Parser String
parseStr = string "123"
       <|> string "12"
       <|> string "1"

parseStr' :: Parser Char
parseStr' = (char '1')
        <|> (char '1' >> char '2')
        <|> (char '1' >> char '2' >> char '3')
