module LiftA2Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Control.Applicative (liftA2)

import Lib
    ( Person (..)
    )

spec :: Spec
spec = do
    describe "constructions" $ do
        it "construction 1" $ do
            let expected = Just $ Person "Doe" "John"
            let lifted = liftA2 Person
            lifted (Just "Doe") (Just "John") `shouldBe` expected
        it "construction 2" $ do
            let expected = Just $ Person "Doe" "John"
            Person <$> (Just "Doe") <*> (Just "John") `shouldBe` expected
