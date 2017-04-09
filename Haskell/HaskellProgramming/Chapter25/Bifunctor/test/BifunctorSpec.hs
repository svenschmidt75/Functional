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
    , SuperDrei (..)
    , SemiDrei (..)
    , Quadriceps (..)
    )


instance (Arbitrary a, Arbitrary b) => Arbitrary (Deux a b) where
    arbitrary = Deux <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Const a b) where
    arbitrary = Const <$> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Drei a b c) where
    arbitrary = Drei <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (SuperDrei a b c) where
    arbitrary = SuperDrei <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (SemiDrei a b c) where
    arbitrary = SemiDrei <$> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Quadriceps a b c d) where
    arbitrary = Quadzzz <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


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

    describe "Drei bifunctor laws" $ do
        prop "first law" (firstProp :: Drei Int Int Int -> Bool)
        prop "second law" (secondProp :: Drei Int Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> Drei Int Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> Drei Int Int Int -> Bool)

    describe "SuperDrei bifunctor laws" $ do
        prop "first law" (firstProp :: SuperDrei Int Int Int -> Bool)
        prop "second law" (secondProp :: SuperDrei Int Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> SuperDrei Int Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> SuperDrei Int Int Int -> Bool)

    describe "SemiDrei bifunctor laws" $ do
        prop "first law" (firstProp :: SemiDrei Int Int Int -> Bool)
        prop "second law" (secondProp :: SemiDrei Int Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> SemiDrei Int Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> SemiDrei Int Int Int -> Bool)

    describe "Quadriceps bifunctor laws" $ do
        prop "first law" (firstProp :: Quadriceps Int Int Int Int -> Bool)
        prop "second law" (secondProp :: Quadriceps Int Int Int Int -> Bool)
        prop "first2 law" (first2Prop :: (Int -> Int) -> (Int -> Int) -> Quadriceps Int Int Int Int -> Bool)
        prop "second2 law" (second2Prop :: (Int -> Int) -> (Int -> Int) -> Quadriceps Int Int Int Int -> Bool)

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
