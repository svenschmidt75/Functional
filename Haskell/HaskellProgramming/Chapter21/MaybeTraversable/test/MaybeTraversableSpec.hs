module MaybeTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Data.Monoid

import Lib
    ( Optional (..)
    )

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = frequency [(1, return Nada), (1, Yep <$> arbitrary)]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Optional Functor laws" $ do
                optionalFunctorLawsProp
            prop "Optional Traversable laws" $ do
                printTestCase "Failure: " optionalTraversableLawsProp

optionalFunctorLawsProp :: Property
optionalFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Optional (Int, Int, Int)
    run $ quickBatch $ functor trigger

optionalTraversableLawsProp :: Property
optionalTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Optional (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
