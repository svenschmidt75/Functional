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
    , activityLogFileSection
    , activityDurationLogFile
    , activitiesPerLogFileSection
    , avgActivityDurationPerDay
    , totalActivityDurationPerDay
    , avgActivityDuration
    ) where

import Data.Char
import Control.Applicative
import qualified Data.Time as DT
import qualified Data.List as DL
import Text.Trifecta
import Text.Parser.LookAhead


type Activity = String


data LogFileEntry = LogFileEntry DT.UTCTime Activity
    deriving Eq

instance Show LogFileEntry where
    show (LogFileEntry t a) = mconcat [timeStr, " ", a, "\n"]
        where
            timeStr = DT.formatTime DT.defaultTimeLocale "%R" t


data LogFileSection = LogFileSection DT.Day [LogFileEntry]
    deriving Eq

instance Show LogFileSection where
    show (LogFileSection d es) = mconcat ["# ", dayStr, "\n", toStr]
        where
            dayStr = DT.formatTime DT.defaultTimeLocale "%F" d
            esStr  = [show e | e <- es]
            toStr  = concat esStr


newtype LogFile = LogFile [LogFileSection]
    deriving Eq

instance Show LogFile where
    show (LogFile ss) = mconcat [show logFileSection | logFileSection <- ss]


-- average time spent per activity per day
avgActivityDuration :: LogFile -> [(DT.Day, [(Activity, DT.NominalDiffTime)])]
avgActivityDuration (LogFile lfss) = map func lfss
    where
        func lfs = (getDay lfs, avgActivityDurationPerDay lfs)

-- average time spent per activity per day
avgActivityDurationPerDay :: LogFileSection -> [(Activity, DT.NominalDiffTime)]
avgActivityDurationPerDay logFileSection = map (\a -> (a, avgDuration a)) activities
    where
        activities = activitiesPerLogFileSection logFileSection
        totalActivityTime = totalActivityDurationPerDay logFileSection
        avgDuration = \activity -> activityLogFileSection activity logFileSection / totalActivityTime

-- total duration of all activities
totalActivityDurationPerDay :: LogFileSection -> DT.NominalDiffTime
totalActivityDurationPerDay logFileSection = foldr (\a b -> b + func a) 0 activities
    where
        activities = activitiesPerLogFileSection logFileSection
        func       = \activity -> activityLogFileSection activity logFileSection

activitiesPerLogFileSection :: LogFileSection -> [Activity]
activitiesPerLogFileSection (LogFileSection _ logFileEntries) = DL.nub $ map (\(LogFileEntry _ a) -> a) logFileEntries

activityDurationLogFile :: Activity -> LogFile -> DT.NominalDiffTime
activityDurationLogFile activity (LogFile lfs) = sum $ map (activityLogFileSection activity) lfs

activityLogFileSection :: Activity -> LogFileSection -> DT.NominalDiffTime
activityLogFileSection activity (LogFileSection _ logFileEntries) =
    foldr (\c b -> b + activityDuration c) 0 matchingEntries
    where
        entryPairs              = zip logFileEntries (tail logFileEntries)
        activityPred (entry, _) = getActivity entry == activity
        matchingEntries         = filter activityPred entryPairs
        activityDuration :: (LogFileEntry, LogFileEntry) -> DT.NominalDiffTime
        activityDuration (LogFileEntry t1 _, LogFileEntry t2 _) = DT.diffUTCTime t2 t1

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
    parseComments
    return $ LogFileEntry time activity

parse2Digits :: Parser Int
parse2Digits = do
    d1 <- digit
    d2 <- digit
    return $ 10 * digitToInt d1 + digitToInt d2

parseTime :: Parser DT.UTCTime
parseTime = do
    -- should use time's parseTime here...
    hr <- parse2Digits
    _ <- char ':'
    m <- parse2Digits
    let secs = toSeconds hr m
    return $ DT.UTCTime (DT.fromGregorian 2000 1 1) (DT.secondsToDiffTime (fromIntegral secs))

parseActivity :: Parser Activity
parseActivity = trim <$> (manyTill anyChar (string "\n" <|> (lookAhead $ string "--")))

parseYear :: Parser Int
parseYear = do
    d1 <- digit
    d2 <- digit
    d3 <- digit
    d4 <- digit
    return $ 1000 * digitToInt d1 + 100 * digitToInt d2 + 10 * digitToInt d3 + digitToInt d4

parseMonth :: Parser Int
parseMonth = parse2Digits

parseDay :: Parser Int
parseDay = parse2Digits

parseUntilEOL :: Parser String
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

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

getDay :: LogFileSection -> DT.Day
getDay (LogFileSection day _) = day
