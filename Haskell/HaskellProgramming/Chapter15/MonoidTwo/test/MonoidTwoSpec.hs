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


{-
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
                    a <- arbitrary
                    return $ Identity a

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
                    a <- arbitrary
                    return $ Sum a
-}

spec :: Spec
spec = do
    describe "Two a b" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: Two (Sum Int)) (b :: Two (Sum Int)) (c :: Two (Sum Int))
        prop "left identity" $
            \a -> monoidLeftIdentity (a :: Two (Sum Int))
        prop "right identity" $
            \a -> monoidRightIdentity (a :: Two (Sum Int))
