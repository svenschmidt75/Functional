module TwoFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Two (..)
    )

spec :: Spec
spec = do
    describe "Two" $ do
        it "foldr" $ do
            let expected = "Test This"
            let is = Two 10 "Test" :: Two Int String
            foldr (++) " This" is `shouldBe` expected
        it "foldMap" $ do
            let expected = "Test"
            let is = Two 10 "Test" :: Two Int String
            foldMap id is `shouldBe` expected
