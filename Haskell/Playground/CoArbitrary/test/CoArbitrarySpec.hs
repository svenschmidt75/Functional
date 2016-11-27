module CoArbitrarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose1 :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose1 f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose2 :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose2 x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

functorCompose3 :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose3 f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

type IntToInt = Fun Int Int
type IntFC = Identity Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Identity Int)
        prop "composition 1" $ do
            let li = functorCompose1 (+1) (*2)
            \x -> li (x :: Identity Int)
        prop "composition 2" $ do
            functorCompose2 :: IntFC
        prop "composition 3" $ do
            functorCompose3 :: IntFC
