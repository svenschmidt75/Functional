module Base10NumberSpec (spec) where

import Test.Hspec

import qualified Text.Trifecta as TF

import Lib
    ( parseDigit
    , base10Integer
    )


spec :: Spec
spec = do
    describe "parseDigit" $ do
        it "Test 1" $ do
            let result = TF.parseString parseDigit mempty "123"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success char-> char `shouldBe` '1'
                -- fail this test...
                TF.Failure _ -> "Parse should have failed on invalid version" `shouldBe` []
        it "Test 2" $ do
            let result = TF.parseString parseDigit mempty "abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _-> "Parse should have failed on invalid version" `shouldBe` []
                -- fail this test...
                TF.Failure _ -> False `shouldBe` False
    describe "base10Integer" $ do
        it "Test 1" $ do
            let result = TF.parseString base10Integer mempty "123abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success r-> r `shouldBe` 123
                -- fail this test...
                TF.Failure _ -> "Parse should have failed on invalid version" `shouldBe` []
        it "Test 2" $ do
            let result = TF.parseString base10Integer mempty "abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success _-> "Parse should have failed on invalid version" `shouldBe` []
                -- fail this test...
                TF.Failure _ -> False `shouldBe` False
        it "Test 3" $ do
            let result = TF.parseString base10Integer mempty "-123abc"
            -- TF.Result does not have an eq instance, so need to
            -- manually unpack
            case result of
                TF.Success r-> r `shouldBe` (-123)
                -- fail this test...
                TF.Failure _ -> "Parse should have failed on invalid version" `shouldBe` []
