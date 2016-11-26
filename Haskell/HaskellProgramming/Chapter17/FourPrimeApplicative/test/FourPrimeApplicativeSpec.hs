module FourPrimeApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Four' (..)
    )


instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Four' a a a b

instance (Eq a, Eq b) => EqProp (Four' a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Four' Functor laws" $ do
                fourPrimeFunctorLawsProp
            prop "Four' Applicative laws" $ do
                fourPrimeApplicativeLawsProp

fourPrimeFunctorLawsProp :: Property
fourPrimeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Four' String (Int, Int, Int)
    run $ quickBatch $ functor trigger

fourPrimeApplicativeLawsProp :: Property
fourPrimeApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Four' String (Int, Int, Int)
    run $ quickBatch $ applicative trigger
