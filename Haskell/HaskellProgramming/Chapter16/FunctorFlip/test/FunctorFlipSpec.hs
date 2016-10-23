{-# LANGUAGE FlexibleInstances #-}

module FunctorFlipSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function

import Lib
    ( Flip (..)
    , K (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type EitherIntFC = Flip Either String Int -> IntToInt -> IntToInt -> Bool

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip Either a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [
                    (1, return $ Flip (Left a))
                  , (1, return $ Flip (Right b))
                  ]

-- instance Arbitrary a => Arbitrary (K a b) where
--     arbitrary = do
--         a <- arbitrary
--         return $ K a

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K a b) where
--     arbitrary = do
--         a <- arbitrary
--         return $ Flip $ a

spec :: Spec
spec = do
    describe "functor Either laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Flip Either String Int)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: Flip Either String Int)
        prop "composition 2" $ do
            functorCompose' :: EitherIntFC

--        it "ds" $ do
--            (+1) <$> (Flip (K 1 :: K Int String)) `shouldBe` (Flip 1 :: K String Int)
--        prop "identity" $ do
  --          \x -> functorIdentity (x :: Flip K String Int)
    --    prop "composition 1" $ do
      --      let li = functorCompose (+1) (*2)
        --    \x -> li (x :: Flip K String Int)
--        prop "composition 2" $ do
  --          functorCompose' :: IntFC
