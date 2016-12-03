module IdentityMonadSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Identity (..)
    )

instance (Arbitrary a) => Arbitrary (Identity a) where
-- arbitrary a :: Gen a
-- Identity <$> arbitrary :: Gen (Identity a)
    arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Identity Functor laws" $ do
                idFunctorLawsProp
            prop "Identity Applicative laws" $ do
                idApplicativeLawsProp
            prop "Identity Monad laws" $ do
                idMonadLawsProp

idFunctorLawsProp :: Property
idFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Identity (Int, Int, Int)
    run $ quickBatch $ functor trigger

idApplicativeLawsProp :: Property
idApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Identity (Int, Int, Int)
    run $ quickBatch $ applicative trigger

idMonadLawsProp :: Property
idMonadLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Identity (Int, Int, Int)
    run $ quickBatch $ monad trigger
