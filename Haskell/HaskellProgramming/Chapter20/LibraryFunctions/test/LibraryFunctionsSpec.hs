module LibraryFunctionsSpec (spec) where

import Prelude hiding (
                        sum
                      , product
                      , elem
                      )
import Test.Hspec

import Lib

spec :: Spec
spec = do
    describe "sum" $ do
        it "test 1" $ do
            let expected = 10
            sum [1, 2, 3, 4 :: Int] `shouldBe` expected
    describe "product" $ do
        it "test 1" $ do
            let expected = 24
            product [1, 2, 3, 4 :: Int] `shouldBe` expected
    describe "elem" $ do
        it "exists" $ do
            elem 1 [1, 2, 3, 4 :: Int] `shouldBe` True
        it "does not exists" $ do
            elem 5 [1, 2, 3, 4 :: Int] `shouldBe` False
