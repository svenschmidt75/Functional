module TrivialMonadSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( W (..)
    , g
    , h
    , join
    )

spec :: Spec
spec = do
    describe "Functor" $ do
        it "fmap" $ do
            let ecpected = W 2 :: W Int
            (+1) <$> (W 1) `shouldBe` ecpected
    describe "Applicative" $ do
        it "pure" $ do
            let expected = W 2 :: W Int
            pure 2 `shouldBe` expected
        it "apply" $ do
            let expected = W 2 :: W Int
            W (+1) <*> W 1 `shouldBe` expected
    describe "Monad" $ do
        it "return" $ do
            let expected = W 2 :: W Int
            pure 2 `shouldBe` expected
        it "bind" $ do
            let expected = W 2 :: W Int
            (W 1 >>= (\x -> W $ x + 1)) `shouldBe` expected
        it "g" $ do
            g 1 (W 2) `shouldBe` W 3
        it "h" $ do
            h (W 1) (W 2) `shouldBe` W 3
        it "join" $ do
            join (W (W 1)) `shouldBe` W 1
