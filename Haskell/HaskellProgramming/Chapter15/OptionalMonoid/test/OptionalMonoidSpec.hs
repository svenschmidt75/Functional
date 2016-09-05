module OptionalMonoidSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Monoid

import Lib
    ( Optional (..)
    )


monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = arbitrarySum

arbitrarySum :: Arbitrary a => Gen (Sum a)
arbitrarySum = do
    a <- arbitrary
    return $ Sum a

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

    describe "verify associativety" $ do
        prop "Sum Int" $
            \a b c -> monoidAssoc (a :: Sum Int) (b :: Sum Int) (c :: Sum Int)

    describe "verify identity" $ do
        prop "Sum Int - left identity" $
            \a -> monoidLeftIdentity (a :: Sum Int)

        prop "Sum Int - right  identity" $
            \a -> monoidRightIdentity (a :: Sum Int)
