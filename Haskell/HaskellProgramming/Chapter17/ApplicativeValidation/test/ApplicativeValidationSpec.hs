module ApplicativeValidationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Validation (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


applicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
applicativeIdentity v = (pure id <*> v) == v

applicativeCompose' :: (Eq (f c), Applicative f) => f a -> Fun a b -> Fun b c -> Bool
applicativeCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

--pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
    deriving (Eq, Show)

type ValidationType = Validation [Errors] Int
type IntToInt = Fun Int Int
type IntFC = ValidationType -> IntToInt -> IntToInt -> Bool

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        a <- arbitrary
        e <- arbitrary
        frequency [
                    (1, return $ Success a)
                  , (1, return $ Failure e)
                  ]

instance Arbitrary Errors where
    arbitrary = frequency [
                            (1, return $ DividedByZero)
                          , (1, return $ StackOverflow)
                          , (1, return $ MooglesChewedWires)
                          ]

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: ValidationType)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: ValidationType)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "some general tests" $ do
        it "Failure - Failure" $ do
            Failure [MooglesChewedWires] <*> Failure [StackOverflow] `shouldBe` (Failure [MooglesChewedWires, StackOverflow] :: ValidationType)
        it "Failure - Success" $ do
            Failure [StackOverflow] <*> Success (+1) `shouldBe` (Failure [StackOverflow] :: ValidationType)
        it "Success - Failure" $ do
            Success (+1) <*> Failure [StackOverflow] `shouldBe` (Failure [StackOverflow] :: ValidationType)
        it "Success - Success" $ do
            Success (+1) <*> Success 1 `shouldBe` (Success 2 :: ValidationType)
    describe "applicative laws" $ do
        prop "identity" $ do
            \x -> applicativeIdentity (x :: ValidationType)
