module SequenceALSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( sequenceAL
    )

spec :: Spec
spec = do
    describe "Maybe Int" $ do
        it "all just" $ do
            let f = [Just 1, Just 2, Just 3]
            let expected = Just [1, 2, 3]
            sequenceAL f `shouldBe` expected
        it "nothing" $ do
            let f = [Nothing] :: [Maybe Int]
            let expected = Nothing
            sequenceAL f `shouldBe` expected
