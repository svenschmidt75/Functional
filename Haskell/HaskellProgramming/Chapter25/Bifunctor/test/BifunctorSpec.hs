{-# LANGUAGE InstanceSigs #-}
module BifunctorSpec (spec) where

import Test.Hspec
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary)
import Test.Hspec.QuickCheck (prop)

import Data.Bifunctor ( first
                      , second)

import Lib
    ( Deux (..)
    )


instance (Arbitrary a, Arbitrary b) => Arbitrary (Deux a b) where
    arbitrary = Deux <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
    describe "Bifunctor laws" $ do
        prop "first law" (deuxFirstProp :: Deux Int Int -> Bool)
        prop "second law" (deuxSecondProp :: Deux Int Int -> Bool)


deuxFirstProp :: (Eq a, Eq b) => Deux a b -> Bool
deuxFirstProp deux = first id deux == id deux

deuxSecondProp :: (Eq a, Eq b) => Deux a b -> Bool
deuxSecondProp deux = second id deux == id deux


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