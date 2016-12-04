module FunctionsWithMonadAndFunctorSpec (spec) where

import Test.Hspec

import Lib
    ( j
    , l1
    , l2
    , a
    , meh
    , flipType
    )

spec :: Spec
spec = do
    describe "j" $ do
        it "test 1" $ do
            let expected = [1, 2, 3] :: [Int]
            j [[1, 2], [], [3]] `shouldBe` expected
        it "test 2" $ do
            let expected = Just 1 :: Maybe Int
            j (Just (Just 1)) `shouldBe` expected
        it "test 3" $ do
            let expected = Nothing :: Maybe Int
            j (Just Nothing) `shouldBe` expected
        it "test 4" $ do
            let expected = Nothing :: Maybe Int
            j Nothing `shouldBe` expected
    describe "l1" $ do
        it "test 1" $ do
            let expected = [1, 2, 3] :: [Int]
            l1 (+1) [0, 1, 2] `shouldBe` expected
    describe "l2" $ do
        it "test 1" $ do
            let expected = Just 5 :: Maybe Int
            l2 (+) (Just 1) (Just 4) `shouldBe` expected
    describe "a" $ do
        it "test 1" $ do
            let expected = Just 2 :: Maybe Int
            a (Just 1) (Just (+1)) `shouldBe` expected
    describe "meh" $ do
        it "test 1" $ do
            let expected = Just [] :: Maybe [Int]
            meh [] return `shouldBe` expected
        it "test 2" $ do
            let expected = Just [1, 2, 3] :: Maybe [Int]
            meh [1, 2, 3] return `shouldBe` expected
    describe "flipType" $ do
        it "test 1" $ do
            let expected = Just [] :: Maybe [Int]
            flipType [] `shouldBe` expected
        it "test 2" $ do
            let expected = Just [1] :: Maybe [Int]
            flipType [Just 1] `shouldBe` expected
