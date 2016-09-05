module OptionalMonoidSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Monoid

import Lib
    ( Optional (Only, Nada)
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


instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        frequency [
                    (1, return $ Only a)
                  , (1, return Nada)
                  ]

spec :: Spec
spec = do
    describe "expected output" $ do
        it "Only(Sum)-Only(Sum)" $ do
            let expr = Only (Sum 1) `mappend` Only (Sum 1)
            let expected = Only Sum {getSum = 2}
            expr `shouldBe` expected

        it "Only(Product)-Only(Product)" $ do
            let expr = Only (Product 4) `mappend` Only (Product 2)
            let expected = Only Product {getProduct = 8}
            expr `shouldBe` expected

        -- this is the equation for the right identity!
        it "Only(Sum)-Nada" $ do
            let expr = Only (Sum 1) `mappend` Nada
            let expected = Only Sum {getSum = 1}
            expr `shouldBe` expected

        it "Only[1]-Nada" $ do
            let expr = Only [1] `mappend` Nada
            let expected = Only [1]
            expr `shouldBe` expected

        -- this is the equation for the left identity!
        it "Nada-Only(Sum)" $ do
            let expr = Nada `mappend` Only (Sum 1)
            let expected = Only Sum {getSum = 1}
            expr `shouldBe` expected

    describe "Sum Int" $ do
        prop "verify associativety" $
            \a b c -> monoidAssoc (a :: Sum Int) (b :: Sum Int) (c :: Sum Int)
        prop "left identity" $
            \a -> monoidLeftIdentity (a :: Sum Int)
        prop "right identity" $
            \a -> monoidRightIdentity (a :: Sum Int)

    describe "Optional" $ do
        prop "verify associativety" $
            \a b c -> monoidAssoc (a :: Optional (Sum Int)) (b :: Optional (Sum Int)) (c :: Optional (Sum Int))
        prop "left identity" $
            \a -> monoidLeftIdentity (a :: Optional(Sum Int))
        prop "right identity" $
            \a -> monoidRightIdentity (a :: Optional(Sum Int))
