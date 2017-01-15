module LogFileSpec (spec) where

import qualified Data.Time as DT
import qualified Text.Trifecta as TF
import Test.Hspec

import Lib
    ( LogFileEntry (..)
    , parseTime
    , parseLogFileEntry
    , parseSectionStart
    )


spec :: Spec
spec = do
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
