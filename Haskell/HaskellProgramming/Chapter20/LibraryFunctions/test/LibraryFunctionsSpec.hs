module LibraryFunctionsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Lib

spec :: Spec
spec = do
    describe "sum" $ do
        it "test 1" $ do
            let expected = 10
            Lib.sum [1, 2, 3, 4 :: Int] `shouldBe` expected
        prop "prop test" prop_sum
    describe "product" $ do
        it "test 1" $ do
            let expected = 24
            Lib.product [1, 2, 3, 4 :: Int] `shouldBe` expected
        prop "prop test" prop_product
    describe "elem" $ do
        it "exists" $ do
            Lib.elem 1 [1, 2, 3, 4 :: Int] `shouldBe` True
        it "does not exists" $ do
            Lib.elem 5 [1, 2, 3, 4 :: Int] `shouldBe` False
        prop "prop test" prop_elem
    describe "minimum" $ do
        it "empty" $ do
            Lib.minimum [] `shouldBe` (Nothing :: Maybe Int)
        it "non-empty" $ do
            Lib.minimum [3, 7, 1, 9, 4, 1, 2, 3, 4, -1 :: Int] `shouldBe` Just (-1)
        prop "prop test" prop_minimum

prop_sum :: [Int] -> Bool
prop_sum xs = Lib.sum xs == Prelude.sum xs

prop_product :: [Int] -> Bool
prop_product xs = Lib.product xs == Prelude.product xs

prop_elem :: Int -> [Int] -> Bool
prop_elem x xs = Lib.elem x xs == Prelude.elem x xs

prop_minimum :: [Int] -> Bool
prop_minimum xs = case Lib.minimum xs of
                    Just a  -> a == Prelude.minimum xs
                    Nothing -> True
