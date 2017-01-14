module PhoneNumbersSpec (spec) where

import Test.Hspec

import qualified Text.Trifecta as TF

import Lib
    ( PhoneNumber (..)
    , parsePhone
    )


spec :: Spec
spec = do
    describe "PhoneNumber Parser" $ do
        it "Test 1" $ do
            let result = TF.parseString parsePhone mempty "123-456-7890"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success pn-> pn `shouldBe` (PhoneNumber 123 456 7890)
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 2" $ do
            let result = TF.parseString parsePhone mempty "1234567890"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success semVer-> semVer `shouldBe` (PhoneNumber 123 456 7890)
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 3" $ do
            let result = TF.parseString parsePhone mempty "(123) 456-7890"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success pn-> pn `shouldBe` (PhoneNumber 123 456 7890)
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
        it "Test 4" $ do
            let result = TF.parseString parsePhone mempty "1-123-456-7890"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success pn-> pn `shouldBe` (PhoneNumber 123 456 7890)
                -- fail this test...
                TF.Failure err   -> show err   `shouldBe` "False"
