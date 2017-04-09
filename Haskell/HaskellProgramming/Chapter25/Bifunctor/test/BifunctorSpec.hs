{-# LANGUAGE InstanceSigs #-}
module BifunctorSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary)
import Test.Hspec.QuickCheck (prop)

import Data.Bifunctor ( Bifunctor
                      , first
                      , second)

import Lib
    ( Deux (..)
    , Const (..)
    , Drei (..)
    )


instance (Arbitrary a, Arbitrary b) => Arbitrary (Deux a b) where
    arbitrary = Deux <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Const a b) where
    arbitrary = Const <$> arbitrary

spec :: Spec
spec = do
    describe "Deux bifunctor laws" $ do
        prop "first law" (firstProp :: Deux Int Int -> Bool)
        prop "second law" (secondProp :: Deux Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> Deux Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> Deux Int Int -> Bool)

    describe "Const bifunctor laws" $ do
        prop "first law" (firstProp :: Const Int Int -> Bool)
        prop "second law" (secondProp :: Const Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> Const Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> Const Int Int -> Bool)

{-
Imported from Data.Bifunctor

Formally, the class Bifunctor represents a bifunctor
 from `Hask` → `Hask`.
Intuitively it is a bifunctor where both the first and second
 arguments are covariant.
You can define a Bifunctor by either defining bimap or by
 defining both first and second.
If you supply bimap, you should ensure that:
    bimap id id ≡ id

If you supply first and second, ensure:
    first id ≡ id
    second id ≡ id

If you supply both, you should also ensure:
    bimap f g ≡ first f . second g

These ensure by parametricity:
    bimap (f . g) (h . i) ≡ bimap f h . bimap g i
    first (f . g) ≡ first f . first g
    second (f . g) ≡ second f . second g

Installed in: global-db
Package: base-4.9.0.0
Defined in: Data.Bifunctor
-}

firstProp :: (Bifunctor f, Eq (f a b)) => f a b -> Bool
firstProp deux = first id deux == id deux

secondProp :: (Bifunctor f, Eq (f a b)) => f a b -> Bool
secondProp deux = second id deux == id deux

first2Prop :: (Bifunctor f, Eq (f d b)) => (c -> d) -> (a -> c) -> f a b -> Bool
first2Prop f g deux = first (f . g) deux == (first f . first g) deux

second2Prop :: (Bifunctor f, Eq (f a d)) => (c -> d) -> (b -> c) -> f a b -> Bool
second2Prop f g deux = second (f . g) deux == (second f . second g) deux
