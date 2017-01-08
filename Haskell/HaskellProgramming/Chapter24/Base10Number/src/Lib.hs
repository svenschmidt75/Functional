module Lib
    ( parseDigit
    , base10Integer
    ) where

import Text.Trifecta


parseDigit :: Parser Char
parseDigit = choice $ char <$> ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

base10Integer :: Parser Integer
base10Integer = do
    sign <- optional $ char '-'
    cs <- some parseDigit
    let s = case sign of Just _  -> (-1)
                         Nothing -> 1
    return $ s * (read cs)