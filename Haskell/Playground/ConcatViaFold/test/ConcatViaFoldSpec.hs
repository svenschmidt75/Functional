module ConcatViaFoldSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( concat'
    )

spec :: Spec
spec = do
    describe "concat' via fold" $ do
        it "test 1" $ do
            let is = [[1], [2], [3], [4], [5]]
            let expected = concat is
            concat' is `shouldBe` expected
        it "test 2" $ do
            let is = [[1, 2], [2], [3], [4], [5]]
            let expected = concat is
            concat' is `shouldBe` expected
