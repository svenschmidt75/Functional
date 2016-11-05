module MonadListConstructorSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( MyList (..)
    )

spec :: Spec
spec = do
    describe "Functor" $ do
        it "empty list" $ do
            let expected = MyList [] :: MyList Int
            (+1) <$> MyList [] `shouldBe` expected
        it "non-empty list" $ do
            let expected = MyList [2, 3] :: MyList Int
            (+1) <$> MyList [1, 2] `shouldBe` expected
    describe "Applicative" $ do
        it "pure" $ do
            let expected = MyList [1] :: MyList Int
            pure 1 `shouldBe` expected
        it "apply" $ do
            let expected = MyList [2, 3] :: MyList Int
            MyList [(+1)] <*> MyList [1, 2] `shouldBe` expected
    describe "conditions" $ do
        it "condition 1" $ do
            let expected = MyList [1, 2] :: MyList Int
            MyList [1, 2] `shouldBe` expected
