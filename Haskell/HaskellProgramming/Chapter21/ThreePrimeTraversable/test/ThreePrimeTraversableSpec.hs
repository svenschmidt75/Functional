module ThreePrimeTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Three' (..)
    )

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Three' Functor laws" $ do
                threeFunctorLawsProp
            prop "Three' Traversable laws" $ do
                threeTraversableLawsProp

threeFunctorLawsProp :: Property
threeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three' Int (Int, Int, Int)
    run $ quickBatch $ functor trigger

threeTraversableLawsProp :: Property
threeTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three' Int (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
