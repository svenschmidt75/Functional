module Lib
    ( LogFileEntry (..)
    , LogFileSection (..)
    , LogFile (..)
    , parseTime
    , parseLogFileEntry
    , parseSectionStart
    , parseLogFile
    , parseLogFileSection
    ) where

import Data.Char
import qualified Data.Time as DT
import Text.Trifecta


type Activity = String

data LogFileEntry = LogFileEntry DT.UTCTime Activity

data LogFileSection = LogFileSection DT.Day [LogFileEntry]

newtype LogFile = LogFile [LogFileSection]


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

skipComment :: Parser ()
skipComment = try $ do
    _ <- string "--"
    skipMany (oneOf "\n")
    return ()

skipComments :: Parser ()
skipComments = do
    whiteSpace
    _ <- optional $ skipComment
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
    skipComments
    return $ DT.fromGregorian (fromIntegral year) month day

parseLogFileSection :: Parser LogFileSection
parseLogFileSection = do
    date <- parseSectionStart
    logFileEntries <- some parseLogFileEntry
    return $ LogFileSection date logFileEntries

parseLogFileEntry :: Parser LogFileEntry
parseLogFileEntry = do
    time <- parseTime
    whiteSpace
    activity <- parseActivity
    return $ LogFileEntry time activity
