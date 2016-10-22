module FunctorTalkToMeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( TalkToMe (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = TalkToMe Int -> IntToInt -> IntToInt -> Bool

instance Arbitrary a => Arbitrary (TalkToMe a) where
    arbitrary = do
        a <- arbitrary
        frequency [
                    (1, return $ Halt)
                  , (1, return $ Print "Hi there" a)
                  -- no one for read as I don't know how to generate functions...
                  ]

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: TalkToMe Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: TalkToMe Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
