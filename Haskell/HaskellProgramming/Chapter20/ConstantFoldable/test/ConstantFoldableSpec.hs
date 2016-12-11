module ConstantFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Constant (..)
    )

spec :: Spec
spec = do
    describe "Constant" $ do
        it "foldr" $ do
            let expected = ""
            let is = Constant 10 :: Constant Int String
            foldr (++) "" is `shouldBe` expected
