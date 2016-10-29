module ApplicativeConstantSpec (spec) where

import Data.Monoid
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( Constant (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = Constant Int Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Constant Int Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: Constant Int Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "applicative" $ do
        it "apply" $ do
            Constant (Sum 1) <*> Constant (Sum 2) `shouldBe` (Constant (Sum 3))
        it "pure" $ do
            (pure 1 :: Constant String Int) `shouldBe` Constant ""
