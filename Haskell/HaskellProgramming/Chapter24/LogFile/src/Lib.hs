module Lib
    -- ( LogFileEntry (..)
    -- , LogFileSection (..)
    -- , LogFile (..)
    -- , parseTime
    -- , parseLogFileEntry
    -- , parseSectionStart
    -- , parseLogFile
    -- , parseLogFileSection
    -- , parseComments
    -- , parseComment
    -- ) where
where

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




activitySum :: Activity -> LogFile -> Int
activitySum = undefined

activityLogFile :: Activity -> LogFile -> Int
activityLogFile = undefined

activityLogFileSection :: Activity -> LogFileSection -> DT.NominalDiffTime
activityLogFileSection activity (LogFileSection _ entries) =
    foldr (\c b -> b + duration c) 0 matchingEntries
    where
        z = zip entries (tail entries)
        activityPred (entry, _) = getActivity entry == activity
        matchingEntries = filter activityPred z
        duration :: (LogFileEntry, LogFileEntry) -> DT.NominalDiffTime
        duration (LogFileEntry t1 _, LogFileEntry t2 _) = DT.diffUTCTime t2 t1

getActivity :: LogFileEntry -> Activity
getActivity (LogFileEntry _ activity) = activity

-- Parse methods

parseLogFile :: Parser LogFile
parseLogFile = do
    logFileSections <- some parseLogFileSection
    eof
    return $ LogFile logFileSections

parseLogFileSection :: Parser LogFileSection
parseLogFileSection = do
    parseComments
    whiteSpace
    date <- parseSectionStart
    logFileEntries <- some parseLogFileEntry
    return $ LogFileSection date logFileEntries

parseLogFileEntry :: Parser LogFileEntry
parseLogFileEntry = do
    whiteSpace
    time <- parseTime
    whiteSpace
    activity <- parseActivity
    whiteSpace
    return $ LogFileEntry time activity

parse2Digits :: Parser Int
parse2Digits = do
    d1 <- digit
    d2 <- digit
    return $ (10 * digitToInt d1) + (digitToInt d2)

parseTime :: Parser DT.UTCTime
parseTime = do
    -- should use time's parseTime here...
    hr <- parse2Digits
    _ <- char ':'
    m <- parse2Digits
    let secs = toSeconds hr m
    return $ DT.UTCTime (DT.fromGregorian 2000 1 1) (DT.secondsToDiffTime (fromIntegral secs))

parseActivity :: Parser Activity
parseActivity = parseUntilEOL

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

parseUntilEOL :: Parser [Char]
parseUntilEOL = some (noneOf "\n")

parseComment :: Parser ()
parseComment = try $ do
    whiteSpace
    _ <- string "--"
    _ <- parseUntilEOL
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

-- Helper methods

toSeconds :: Int -> Int -> Int
toSeconds h m = h * 3600 + m * 60
