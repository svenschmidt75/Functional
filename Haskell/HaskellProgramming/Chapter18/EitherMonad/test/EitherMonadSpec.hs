module EitherMonadSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Sum (..)
    )

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary = frequency [
                            (1, First <$> arbitrary)
                          , (1, Second <$> arbitrary)
                          ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Sum Functor laws" $ do
                sumFunctorLawsProp
            prop "Sum Applicative laws" $ do
                sumApplicativeLawsProp
            prop "Sum Monad laws" $ do
                sumMonadLawsProp

sumFunctorLawsProp :: Property
sumFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Sum String (Int, Int, Int)
    run $ quickBatch $ functor trigger

sumApplicativeLawsProp :: Property
sumApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Sum String (Int, Int, Int)
    run $ quickBatch $ applicative trigger

sumMonadLawsProp :: Property
sumMonadLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Sum String (Int, Int, Int)
    run $ quickBatch $ monad trigger
