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
    describe "Monad" $ do
        it "return" $ do
            let expected = MyList [1] :: MyList Int
            return 1 `shouldBe` expected
        it "bind" $ do
            let expected = MyList [2, 3, 3, 4] :: MyList Int
            (MyList [1, 2] >>= (\x -> MyList $ [x + 1, x + 2])) `shouldBe` expected
