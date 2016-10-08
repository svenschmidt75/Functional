module MonoidTrivialSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup
import Data.Monoid hiding ((<>))

import Lib
    ( Trivial (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Arbitrary Trivial where
    arbitrary = return Trivial

spec :: Spec
spec = do
    describe "Sum Int" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: Trivial) (b :: Trivial) (c :: Trivial)
        prop "left identity" $
            \a -> monoidLeftIdentity (a :: Trivial)
        prop "right identity" $
            \a -> monoidRightIdentity (a :: Trivial)
