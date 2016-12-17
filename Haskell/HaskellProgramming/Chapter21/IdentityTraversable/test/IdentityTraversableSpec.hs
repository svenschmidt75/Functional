module IdentityTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Data.Monoid

import Lib
    ( Identity (..)
    )

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Identity Functor laws" $ do
                identityFunctorLawsProp
            prop "Identity Traversable laws" $ do
                printTestCase "Failure: " identityTraversableLawsProp

identityFunctorLawsProp :: Property
identityFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Identity (Int, Int, Int)
    run $ quickBatch $ functor trigger

identityTraversableLawsProp :: Property
identityTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Identity (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
