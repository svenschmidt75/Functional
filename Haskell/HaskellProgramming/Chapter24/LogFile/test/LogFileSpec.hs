{-# LANGUAGE QuasiQuotes #-}
module LogFileSpec (spec) where

import qualified Data.Time as DT
import qualified Text.Trifecta as TF
import Text.RawString.QQ
import Test.Hspec

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
08:00 Breakfast -- should I try skippin bfast?CHAPTER 24. PARSER COMBINATORS
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
08:00 Breakfast -- should I try skippin bfast?CHAPTER 24. PARSER COMBINATORS
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
            let result = TF.parseString parseLogFileEntry mempty "08:00 Breakfast"
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