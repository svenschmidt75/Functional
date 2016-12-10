module LibraryFunctionsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Monoid

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
    describe "maximum" $ do
        it "empty" $ do
            Lib.maximum [] `shouldBe` (Nothing :: Maybe Int)
        it "non-empty" $ do
            Lib.maximum [3, 7, 1, 9, 4, 1, 2, 3, 4, -1 :: Int] `shouldBe` Just 9
        prop "prop test" prop_maximum
    describe "null" $ do
        it "empty" $ do
            Lib.null [] `shouldBe` True
        it "non-empty" $ do
            Lib.null [3, -1 :: Int] `shouldBe` False
        prop "prop test" prop_null
    describe "length" $ do
        it "empty" $ do
            Lib.length [] `shouldBe` 0
        it "non-empty" $ do
            Lib.length [3, -1 :: Int] `shouldBe` 2
        prop "prop test" prop_length
    describe "toList" $ do
        it "Maybe" $ do
            Lib.toList (Just 1 :: Maybe Int) `shouldBe` [1]
        it "tuple" $ do
            Lib.toList (1 :: Int, 2 :: Int) `shouldBe` [2 :: Int]
    describe "fold" $ do
        it "Sum" $ do
            Lib.fold [1, 2 :: Sum Int] `shouldBe` Sum 3
        it "Product" $ do
            Lib.fold [1, 8, 3 :: Product Int] `shouldBe` Product 24
    describe "foldMap" $ do
        it "Sum" $ do
            Lib.foldMap Sum [1, 2 :: Int] `shouldBe` Sum 3
        it "Product" $ do
            Lib.foldMap Product [1, 8, 3 :: Int] `shouldBe` Product 24

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

prop_maximum :: [Int] -> Bool
prop_maximum xs = case Lib.maximum xs of
                    Just a  -> a == Prelude.maximum xs
                    Nothing -> True

prop_null :: [Int] -> Bool
prop_null xs = Lib.null xs == Prelude.null xs

prop_length :: [Int] -> Bool
prop_length xs = Lib.length xs == Prelude.length xs
