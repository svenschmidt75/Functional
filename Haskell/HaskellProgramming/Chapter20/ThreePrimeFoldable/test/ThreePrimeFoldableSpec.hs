module ThreePrimeFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Three' (..)
    )

spec :: Spec
spec = do
    describe "Three'" $ do
        it "foldr" $ do
            let expected = "Test 2 This"
            let is = Three' 10 "Test 1" "Test 2" :: Three' Int String
            foldr (++) " This" is `shouldBe` expected
        it "foldMap" $ do
            let expected = "Test 2"
            let is = Three' 10 "Test 1" "Test 2" :: Three' Int String
            foldMap id is `shouldBe` expected
