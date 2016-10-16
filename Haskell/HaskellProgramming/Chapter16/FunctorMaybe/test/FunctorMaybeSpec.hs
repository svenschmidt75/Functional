module FunctorMaybeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( Possibly (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


type IntToInt = Fun Int Int
type IntFC = Possibly Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Possibly a) where
    arbitrary = do
        a <- arbitrary
        frequency
                    [
                      (1, return $ Yeppers a)
                    , (1, return LolNope)
                    ]

showMaybe :: (Functor f, Show a) => f a -> f String
showMaybe = fmap show

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Possibly Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: Possibly Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "Maybe Int Functor" $ do
        it "Yeppers" $ do
            let result = fmap (+1) (Yeppers 1)
            result `shouldBe` Yeppers 2
        it "LolNope" $ do
            let result = fmap (+1) LolNope
            result `shouldBe` LolNope
    describe "Maybe String Functor" $ do
        it "Yeppers" $ do
            let result = showMaybe [1..5]
            result `shouldBe` ["1", "2", "3", "4", "5"]
        it "LolNope" $ do
            let result = showMaybe (LolNope :: Possibly [Int])
            result `shouldBe` LolNope
