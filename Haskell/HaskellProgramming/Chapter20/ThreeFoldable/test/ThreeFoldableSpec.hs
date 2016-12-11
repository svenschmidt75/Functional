module ThreeFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Three (..)
    )

spec :: Spec
spec = do
    describe "Three" $ do
        it "foldr" $ do
            let expected = "Test This"
            let is = Three 10 (Just 10) "Test" :: Three Int (Maybe Int) String
            foldr (++) " This" is `shouldBe` expected
        it "foldMap" $ do
            let expected = "Test"
            let is = Three 10 (Just 10) "Test" :: Three Int (Maybe Int) String
            foldMap id is `shouldBe` expected
