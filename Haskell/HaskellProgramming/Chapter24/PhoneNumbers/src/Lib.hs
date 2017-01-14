module Lib
    ( PhoneNumber (..)
    , parsePhone
    ) where

import Data.Char
import Control.Applicative
import Text.Trifecta


type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parseCountryCode :: Parser (Maybe ())
parseCountryCode = do
    optional $ try $ do
        _ <- char '1' >> char '-'
        return ()

parse3Digits :: Parser Int
parse3Digits = do
    d1 <- digit
    d2 <- digit
    d3 <- digit
    return $ 100 * (digitToInt d1) + 10 * (digitToInt d2) + (digitToInt d3)

parse4Digits :: Parser Int
parse4Digits = do
    d1 <- digit
    d2 <- digit
    d3 <- digit
    d4 <- digit
    return $ 1000 * (digitToInt d1) + 100 * (digitToInt d2) + 10 * (digitToInt d3) + (digitToInt d4)

parseNPA :: Parser NumberingPlanArea
parseNPA =  parse3Digits
        <|> parens parse3Digits

parseExchange :: Parser Exchange
parseExchange = do
    whiteSpace
    parse3Digits <|> (char '-' >> parse3Digits)

parseLineNumber :: Parser LineNumber
parseLineNumber = do
    parse4Digits <|> (char '-' >> parse4Digits)

parsePhone :: Parser PhoneNumber
parsePhone = do
    _ <- parseCountryCode
    npa <- parseNPA
    exg <- parseExchange
    ln <- parseLineNumber
    return $ PhoneNumber npa exg ln
