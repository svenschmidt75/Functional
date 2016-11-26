module ThreePrimeApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Three' (..)
    )


instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Three' a b b

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Three' Functor laws" $ do
                threePrimeFunctorLawsProp
            prop "Three' Applicative laws" $ do
                threePrimeApplicativeLawsProp

threePrimeFunctorLawsProp :: Property
threePrimeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three' String (Int, Int, Int)
    run $ quickBatch $ functor trigger

threePrimeApplicativeLawsProp :: Property
threePrimeApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three' String (Int, Int, Int)
    run $ quickBatch $ applicative trigger
