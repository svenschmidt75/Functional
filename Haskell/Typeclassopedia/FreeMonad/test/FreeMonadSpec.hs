module FreeMonadSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( Free (..)
    )

spec :: Spec
spec = do
    describe "Functor" $ do
        it "fmap" $ do
            let expected = Free $ Just 1
            ((+1) <$> Free $ Just 1) `shouldBe` expected
