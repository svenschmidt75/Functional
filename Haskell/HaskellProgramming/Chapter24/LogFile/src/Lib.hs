module Lib
    ( LogFileEntry (..)
    , LogFileSection (..)
    , LogFile (..)
    , parseTime
    , parseLogFileEntry
    , parseSectionStart
    , parseLogFile
    , parseLogFileSection
    , parseComments
    , parseComment
    ) where

import Data.Char
import qualified Data.Time as DT
import Text.Trifecta


type Activity = String

data LogFileEntry = LogFileEntry DT.UTCTime Activity
    deriving (Show, Eq)

data LogFileSection = LogFileSection DT.Day [LogFileEntry]
    deriving (Show, Eq)

newtype LogFile = LogFile [LogFileSection]
    deriving (Show, Eq)


parse2Digits :: Parser Int
parse2Digits = do
    d1 <- digit
    d2 <- digit
    return $ (10 * digitToInt d1) + (digitToInt d2)

toSeconds :: Int -> Int -> Int
toSeconds h m = h * 3600 + m * 60

parseTime :: Parser DT.UTCTime
parseTime = do
    -- should use time's parseTime here...
    hr <- parse2Digits
    _ <- char ':'
    m <- parse2Digits
    let secs = toSeconds hr m
    return $ DT.UTCTime (DT.fromGregorian 2000 1 1) (DT.secondsToDiffTime (fromIntegral secs))

parseActivity :: Parser Activity
parseActivity = some anyChar

parseLogFile :: Parser LogFile
parseLogFile = undefined

parseYear :: Parser Int
parseYear = do
    d1 <- digit
    d2 <- digit
    d3 <- digit
    d4 <- digit
    return $ (1000 * digitToInt d1) + (100 * digitToInt d2) + (10 * digitToInt d3) + (digitToInt d4)

parseMonth :: Parser Int
parseMonth = parse2Digits

parseDay :: Parser Int
parseDay = parse2Digits

parseComment :: Parser ()
parseComment = try $ do
    whiteSpace
    _ <- string "--"
    skipMany (oneOf "\n")
    return ()

parseComments :: Parser ()
parseComments = do
    whiteSpace
    _ <- optional parseComment
    return ()

parseSectionStart :: Parser DT.Day
parseSectionStart = do
    _ <- char '#'
    whiteSpace
    year <- parseYear
    _ <- char '-'
    month <- parseMonth
    _ <- char '-'
    day <- parseDay
    parseComments
    return $ DT.fromGregorian (fromIntegral year) month day

parseLogFileSection :: Parser LogFileSection
parseLogFileSection = do
    parseComments
    date <- parseSectionStart
    whiteSpace
    logFileEntries <- some parseLogFileEntry
    return $ LogFileSection date logFileEntries

parseLogFileEntry :: Parser LogFileEntry
parseLogFileEntry = do
    time <- parseTime
    whiteSpace
    activity <- parseActivity
    return $ LogFileEntry time activity
