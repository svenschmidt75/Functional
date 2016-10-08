module AccumulateBothSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup

import Lib
    ( AccumulateBoth (..)
    , Validation' (Failure', Success')
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
                    a <- arbitrary
                    return $ Sum a

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
    arbitrary = do
                    a <- arbitrary
                    b <- arbitrary
                    frequency [
                                (1, return $ AccumulateBoth $ Failure' a)
                              , (1, return $ AccumulateBoth $ Success' b)
                              ]


spec :: Spec
spec = do
    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: AccumulateBoth String (Sum Int)) (b :: AccumulateBoth String (Sum Int)) (c :: AccumulateBoth String (Sum Int))
