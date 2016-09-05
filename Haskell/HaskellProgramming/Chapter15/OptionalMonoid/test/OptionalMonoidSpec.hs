module OptionalMonoidSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Monoid

import Lib
    ( Optional (..)
    )


spec :: Spec
spec = do
    describe "expected output" $ do
        it "Only(Sum)-Only(Sum)" $ do
            let expr = Only (Sum 1) `mappend` Only (Sum 1)
            let expected = Only (Sum {getSum = 2})
            expr `shouldBe` expected

        it "Only(Product)-Only(Product)" $ do
            let expr = Only (Product 4) `mappend` Only (Product 2)
            let expected = Only (Product {getProduct = 8})
            expr `shouldBe` expected

        it "Only(Sum)-Nada" $ do
            let expr = Only (Sum 1) `mappend` Nada
            let expected = Only (Sum {getSum = 1})
            expr `shouldBe` expected

        it "Only[1]-Nada" $ do
            let expr = Only [1] `mappend` Nada
            let expected = Only [1]
            expr `shouldBe` expected

        it "Nada-Only(Sum)" $ do
            let expr = Nada `mappend` Only (Sum 1)
            let expected = Only (Sum {getSum = 1})
            expr `shouldBe` expected
