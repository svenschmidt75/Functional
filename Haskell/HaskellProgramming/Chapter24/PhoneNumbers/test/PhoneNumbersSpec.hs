module PhoneNumbersSpec (spec) where

import Test.Hspec

import qualified Text.Trifecta as TF

import Lib
    ( SemVer (..)
    , NumberOrString (..)
    , parseSemVer
    )


spec :: Spec
spec = do
    describe "SemVerParser" $ do
        it "Test 1 - malformed version" $ do
            let result = TF.parseString parseSemVer mempty "2.11"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _-> "Parse should have failed on invalid version" `shouldBe` []
                -- fail this test...
                TF.Failure _ -> False `shouldBe` False
        it "Test 2" $ do
            let result = TF.parseString parseSemVer mempty "2.1.1"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success semVer-> semVer `shouldBe` (SemVer 2 1 1 [] [])
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 3" $ do
            let result = TF.parseString parseSemVer mempty "1.0.0-x.7.z.92"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success semVer-> semVer `shouldBe` (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 4" $ do
            let result = TF.parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success semVer-> semVer `shouldBe` (SemVer 1 0 0 [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85"])
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 5" $ do
            SemVer 2 1 1 [] [] > SemVer 2 1 0 [] [] `shouldBe` True
