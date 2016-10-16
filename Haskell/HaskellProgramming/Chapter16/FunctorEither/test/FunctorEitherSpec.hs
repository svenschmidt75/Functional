module FunctorEitherSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( Sum (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


type IntToInt = Fun Int Int
type IntFC = Sum String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency
                    [
                      (1, return $ First a)
                    , (1, return $ Second b)
                    ]

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Sum String Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: Sum String Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "Either String Int Functor" $ do
        it "First" $ do
            let result = fmap (+1) (First "Test")
            result `shouldBe` First "Test"
        it "Second" $ do
            let result = fmap (+1) (Second 1)
            result `shouldBe` (Second 2 :: Sum String Int)
