module ValidationApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Validation (..)
    )

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        frequency [(1, return $ Failure e), (1, return $ Success a)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
    (=-=) = eq

spec :: Spec
spec = do
    prop "List Applicative laws" $ do
        listApplicativeProp


listApplicativeProp :: Property
listApplicativeProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Validation String (Int, Int, Int)
    run $ quickBatch $ functor trigger
