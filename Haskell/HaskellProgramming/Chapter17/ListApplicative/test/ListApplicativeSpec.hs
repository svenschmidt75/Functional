module ListApplicativeSpec (spec) where

import Data.Semigroup

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( List (..)
    )

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = List Int -> IntToInt -> IntToInt -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ Cons a Nil)
                 , (1, return $ Nil)]

spec :: Spec
spec = do
    describe "List Functor Laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: List Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: List Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "Applicative Tests" $ do
        it "test 1" $ do
            let functions = Cons (+1) Nil
            let values = Cons 1 Nil
            let result = functions <*> values
            result `shouldBe` (Cons 2 Nil)
        it "test 1" $ do
            let functions = Cons (+1) (Cons (*2) Nil)
            let values = Cons 1 (Cons 2 Nil)
            let result = functions <*> values
            result `shouldBe` (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))
