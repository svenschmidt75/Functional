module HelperSpec (spec) where

import Helper
       ( replace
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "replace" $ do
        it "replace - does not exist" $ do
            let input = [1, 2, 3, 4]
            replace 7 1 input `shouldBe` input

        it "replace - does exist" $ do
            let input = [1, 2, 3, 4]
            let expected = [1, 2, 7, 4]
            replace 3 7 input `shouldBe` expected

        it "replace - at beginning" $ do
            let input = [1, 2, 3, 4]
            let expected = [7, 2, 3, 4]
            replace 1 7 input `shouldBe` expected

        it "replace - at end" $ do
            let input = [1, 2, 3, 4]
            let expected = [1, 2, 3, 7]
            replace 4 7 input `shouldBe` expected

        it "replace - multiple ocurances" $ do
            let input = [1, 2, 3, 4, 3, 6, 2, 4, 8]
            let expected = [1, 9, 3, 4, 3, 6, 9, 4, 8]
            replace 2 9 input `shouldBe` expected
