module FunctorIgnoreOneSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( IgnoreOne (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = IgnoreOne Maybe Maybe Int Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ IgnoringSomething a b

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: IgnoreOne Maybe Maybe Int Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: IgnoreOne Maybe Maybe Int Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
