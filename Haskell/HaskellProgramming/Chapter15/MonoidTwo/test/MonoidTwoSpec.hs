module MonoidTwoSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup
import Data.Monoid hiding ((<>))

import Lib
    ( Two (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return $ Sum a

type TwoType = Two String (Sum Int)
type TwoAssoc = TwoType -> TwoType -> TwoType -> Bool

spec :: Spec
spec = do
    describe "Two a b" $ do
        prop "verify associativety" $ do
            semigroupAssoc :: TwoAssoc
        prop "left identity" $ do
            monoidLeftIdentity :: TwoType -> Bool
        prop "right identity" $ do
            monoidRightIdentity :: TwoType -> Bool
