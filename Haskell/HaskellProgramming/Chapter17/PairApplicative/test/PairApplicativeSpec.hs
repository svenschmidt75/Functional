module PairApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Pair (..)
    )


instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        return $ Pair a a

instance Eq a => EqProp (Pair a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Pair Functor laws" $ do
                pairFunctorLawsProp
            prop "Pair Applicative laws" $ do
                pairApplicativeLawsProp

pairFunctorLawsProp :: Property
pairFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Pair (Int, Int, Int)
    run $ quickBatch $ functor trigger

pairApplicativeLawsProp :: Property
pairApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Pair (Int, Int, Int)
    run $ quickBatch $ applicative trigger
