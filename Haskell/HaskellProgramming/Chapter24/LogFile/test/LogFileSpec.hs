{-# LANGUAGE QuasiQuotes #-}
module LogFileSpec (spec) where

import qualified Data.Time as DT
import qualified Text.Trifecta as TF
import Text.RawString.QQ

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Lib
    ( LogFileEntry (..)
    , LogFileSection (..)
    , LogFile (..)
    , parseComments
    , parseComment
    , parseTime
    , parseLogFileEntry
    , parseSectionStart
    , parseLogFileSection
    , parseLogFile
    , activityLogFileSection
    , activityDurationLogFile
    , activitiesPerLogFileSection
    , totalActivityDurationPerDay
    )

exampleLog :: String
exampleLog = [r|
-- wheee a comment
# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]

exampleLogFileSection :: String
exampleLogFileSection = [r|
# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]


instance Arbitrary DT.UTCTime where
    arbitrary = do
        year <- choose (1000, 2000)
        day <- choose (1, 31)
        month <- choose (1, 31)
        hour <- choose (0, 11)
        minute <- choose (0, 59)
        let secs = (hour * 3600 + minute * 60) :: Integer
        return $ DT.UTCTime (DT.fromGregorian year month day) (DT.secondsToDiffTime (fromIntegral secs))

instance Arbitrary DT.Day where
    arbitrary = do
        year <- choose (1000, 2000)
        day <- choose (1, 31)
        month <- choose (1, 31)
        return $ DT.fromGregorian year month day

generateActivity :: Gen String
generateActivity = do
    rndActivity <- filter (/= '\n') <$> arbitrary
    -- we append an 'a' here, to make sure the activity
    -- is never an empty string!
    return $ rndActivity ++ "a\n"

instance Arbitrary LogFileEntry where
    arbitrary = LogFileEntry <$> arbitrary <*> generateActivity

instance Arbitrary LogFileSection where
    arbitrary = sized $ \n -> do
        -- We have to put sequence here, because
        -- [arbitrary | _ <- [1..n]] :: [Gen LogFileEntry], but we
        -- need Gen [LogFileEntry]!
        let logFileEntries = sequence [arbitrary | _ <- [1..n]]
        LogFileSection <$> arbitrary <*> logFileEntries

spec :: Spec
spec = do
    describe "parseComment" $ do
        it "Test 1" $ do
            let result = TF.parseString parseComment mempty "-- abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> True `shouldBe` True
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "leading newline" $ do
            let result = TF.parseString parseComment mempty "\n-- abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> True `shouldBe` True
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
        it "leading newline" $ do
            let result = TF.parseString parseComments mempty "\n-- abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> True `shouldBe` True
                -- fail this test...
                TF.Failure err -> show err `shouldBe` "False"
    describe "parseTime" $ do
        it "Test 1" $ do
            let result = TF.parseString parseTime mempty "08:00"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success (DT.UTCTime utctDay utctDayTime)
                    -> (DT.toGregorian utctDay, utctDayTime) `shouldBe` (expectedDay, expectedTime)
                        where
                            expectedDay  = (2000, 1, 1)
                            expectedTime = 8 * 3600
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parseTime mempty "21:15"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success (DT.UTCTime utctDay utctDayTime)
                    -> (DT.toGregorian utctDay, utctDayTime) `shouldBe` (expectedDay, expectedTime)
                        where
                            expectedDay  = (2000, 1, 1)
                            expectedTime = 21 * 3600 + 15 * 60
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
    describe "parseLogFileEntry" $ do
        it "Test 1" $ do
            let result = TF.parseString parseLogFileEntry mempty "08:00 Breakfast\n"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success (LogFileEntry time activity)
                    -> (time, activity) `shouldBe` (expectedTime, expectedActivity)
                        where
                            expectedTime = (DT.UTCTime (DT.fromGregorian 2000 1 1) (DT.secondsToDiffTime $ 8 * 3600))
                            expectedActivity = "Breakfast"
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parseLogFileEntry mempty "08:00 Breakfast -- with comment"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success (LogFileEntry time activity)
                    -> (time, activity) `shouldBe` (expectedTime, expectedActivity)
                        where
                            expectedTime = (DT.UTCTime (DT.fromGregorian 2000 1 1) (DT.secondsToDiffTime $ 8 * 3600))
                            expectedActivity = "Breakfast"
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
        prop "Test 3 - from generated" $
--            modifyMaxSuccess (const 1) $
                generatedLogFileEntryProp
    describe "parseSectionStart" $ do
        it "Test 1" $ do
            let result = TF.parseString parseSectionStart mempty "# 2025-02-05"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success day
                    -> day `shouldBe` expectedDay
                        where
                            expectedDay = DT.fromGregorian 2025 2 5
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parseSectionStart mempty "# 2025-02-07 -- dates not nececessarily sequential"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success day
                    -> day `shouldBe` expectedDay
                        where
                            expectedDay = DT.fromGregorian 2025 2 7
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"

    describe "parseLogFileSection" $ do
        it "Test 1" $ do
            let result = TF.parseString parseLogFileSection mempty exampleLogFileSection
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _ -> True `shouldBe` True
                    -- -> day `shouldBe` expectedDay
                    --     where
                    --         expectedDay = DT.fromGregorian 2025 2 5
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"
        prop "Test 2 - from generated" $
--            modifyMaxSuccess (const 1) $
                generatedLogFileSectionProp

    describe "parseLogFile" $ do
        it "Test 1" $ do
            let result = TF.parseString parseLogFile mempty exampleLog
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success (LogFile logFileSections) -> (length logFileSections) `shouldBe` 2
                    -- -> day `shouldBe` expectedDay
                    --     where
                    --         expectedDay = DT.fromGregorian 2025 2 5
                -- fail this test...
                TF.Failure err   -> show err `shouldBe` "False"

    describe "activityLogFileSection" $ do
        it "Test 1" $ do
            let lfe1 = LogFileEntry (DT.UTCTime (DT.fromGregorian 2017 1 1) 61) "Breakfast"
            let lfe2 = LogFileEntry (DT.UTCTime (DT.fromGregorian 2017 1 1) 120) "Reading"
            let lfs = LogFileSection (DT.fromGregorian 2017 1 1) [lfe1, lfe2]
            let result = activityLogFileSection "Breakfast" lfs
            result `shouldBe` 59
        it "Test 2 - Breakfast" $ do
            let result = TF.parseString parseLogFileSection mempty exampleLogFileSection
            case result of
                TF.Success lfs -> do
                    -- Breakfast for 1hr = 60*60=3600 seconds
                    let duration = activityLogFileSection "Breakfast" lfs
                    duration `shouldBe` 60*60
                TF.Failure err   -> show err `shouldBe` "False"
        it "Test 3 - Read" $ do
            let result = TF.parseString parseLogFileSection mempty exampleLogFileSection
            case result of
                TF.Success lfs -> do
                    let duration = activityLogFileSection "Read" lfs
                    -- read for 7.5hr = 7.5*60*60=27000 seconds
                    duration `shouldBe` 7.5*60*60
                TF.Failure err   -> show err `shouldBe` "False"
--         prop "5" $ do
-- --            modifyMaxSuccess (const 1) $ do
--                 printLogFileSectionProp
--         prop "6" $ do
-- --            modifyMaxSuccess (const 1) $ do
--                 printLogFileProp

    describe "activityDurationLogFile" $ do
        it "Test 1 - Breakfast" $ do
            let result = TF.parseString parseLogFile mempty exampleLog
            case result of
                TF.Success lfs -> do
                    -- Breakfast for 1hr = 60*60=3600 seconds at two days
                    let duration = activityDurationLogFile "Breakfast" lfs
                    duration `shouldBe` 2*60*60
                TF.Failure err   -> show err `shouldBe` "False"
        it "Test 1 - Dinner" $ do
            let result = TF.parseString parseLogFile mempty exampleLog
            case result of
                TF.Success lfs -> do
                    -- Dinner for 2hr = 2*60*60 seconds, and 15min
                    let duration = activityDurationLogFile "Dinner" lfs
                    duration `shouldBe` 2*60*60+15*60
                TF.Failure err   -> show err `shouldBe` "False"

    describe "activitiesPerLogFileSection" $ do
        it "Test 1" $ do
            let parseResult = TF.parseString parseLogFileSection mempty exampleLogFileSection
            let result = (length . activitiesPerLogFileSection) <$> parseResult
            case result of
                TF.Success count -> do
                    count `shouldBe` 9
                TF.Failure err   -> show err `shouldBe` "False"

    describe "totalActivityDurationPerDay" $ do
        it "Test 1" $ do
            let parseResult = TF.parseString parseLogFileSection mempty exampleLogFileSection
            let result = totalActivityDurationPerDay <$> parseResult
            case result of
                TF.Success count -> do
                    -- 14hr total activity
                    count `shouldBe` (14*60*60)
                TF.Failure err   -> show err `shouldBe` "False"

-- -- Write some output while running tests
-- printLogFileSectionProp :: Property
-- printLogFileSectionProp = monadicIO $ do
--     let result = TF.parseString parseLogFileSection mempty exampleLogFileSection
--     run $ case result of
--         TF.Success lfs -> print lfs
--         TF.Failure err   -> return ()

-- printLogFileProp :: Property
-- printLogFileProp = monadicIO $ do
--     let result = TF.parseString parseLogFile mempty exampleLog
--     run $ case result of
--         TF.Success lfs -> print lfs
--         TF.Failure err   -> return ()

generatedLogFileEntryProp :: Property
generatedLogFileEntryProp = monadicIO $ do
    let logFileEntry = show <$> (arbitrary :: Gen LogFileEntry)
    argument <- run $ generate logFileEntry
    -- show the failure if any
--    run $ print argument
    let result = TF.parseString parseLogFileEntry mempty argument
    case result of
        (TF.Success _) -> assert True
        _              -> assert False

generatedLogFileSectionProp :: Property
generatedLogFileSectionProp = monadicIO $ do
    let logFileSection = show <$> (arbitrary :: Gen LogFileSection)
    argument <- run $ generate logFileSection
    -- show the failure if any
--    run $ print argument
    let result = TF.parseString parseLogFileSection mempty argument
    case result of
        (TF.Success _) -> assert True
        _              -> assert False
