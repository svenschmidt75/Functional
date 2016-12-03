module NopeMonadSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Nope (..)
    )

instance Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Nope Functor laws" $ do
                nopeFunctorLawsProp
            prop "Nope Applicative laws" $ do
                nopeApplicativeLawsProp
            prop "Nope Monad laws" $ do
                nopeMonadLawsProp

nopeFunctorLawsProp :: Property
nopeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Nope (Int, Int, Int)
    run $ quickBatch $ functor trigger

nopeApplicativeLawsProp :: Property
nopeApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Nope (Int, Int, Int)
    run $ quickBatch $ applicative trigger

nopeMonadLawsProp :: Property
nopeMonadLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Nope (Int, Int, Int)
    run $ quickBatch $ monad trigger
