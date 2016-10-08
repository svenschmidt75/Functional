module AccumulateRightSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup

import Lib
    ( AccumulateRight (..)
    , Validation' (Failure', Success')
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
                    a <- arbitrary
                    return $ Sum a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
    arbitrary = do
                    a <- arbitrary
                    b <- arbitrary
                    frequency [
                                (1, return $ AccumulateRight $ Failure' a)
                              , (1, return $ AccumulateRight $ Success' b)
                              ]


spec :: Spec
spec = do
    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: AccumulateRight String (Sum Int)) (b :: AccumulateRight String (Sum Int)) (c :: AccumulateRight String (Sum Int))
