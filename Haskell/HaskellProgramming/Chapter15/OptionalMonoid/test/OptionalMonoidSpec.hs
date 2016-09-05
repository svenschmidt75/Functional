module OptionalMonoidSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "solver" $ do
        it "simple triangle" $ do
            1 `shouldBe` 1
