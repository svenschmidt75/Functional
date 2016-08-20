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
