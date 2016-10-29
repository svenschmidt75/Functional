module ApplicativeLookupSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( added
    , tupled
    , maxed
    , summed
    )

spec :: Spec
spec = do
    describe "lookup exercises" $ do
        it "added" $ do
            added `shouldBe` (Just 9)
        it "tupled" $ do
            tupled `shouldBe` Just (6, 5)
        it "maxed" $ do
            maxed `shouldBe` Just 3
        it "summed" $ do
            summed `shouldBe` Just 5
