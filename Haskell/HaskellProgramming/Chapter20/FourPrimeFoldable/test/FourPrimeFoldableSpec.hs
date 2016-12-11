module FourPrimeFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Four' (..)
    )

spec :: Spec
spec = do
    describe "Four'" $ do
        it "foldr" $ do
            let expected = "Test 3 This"
            let is = Four' 10 "Test 1" "Test 2" "Test 3" :: Four' Int String
            foldr (++) " This" is `shouldBe` expected
        it "foldMap" $ do
            let expected = "Test 3"
            let is = Four' 10 "Test 1" "Test 2" "Test 3" :: Four' Int String
            foldMap id is `shouldBe` expected
