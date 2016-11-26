module ThreeApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Three (..)
    )


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Three Functor laws" $ do
                threeFunctorLawsProp
            prop "Three Applicative laws" $ do
                threeApplicativeLawsProp

threeFunctorLawsProp :: Property
threeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three String String (Int, Int, Int)
    run $ quickBatch $ functor trigger

threeApplicativeLawsProp :: Property
threeApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Three String String (Int, Int, Int)
    run $ quickBatch $ applicative trigger
