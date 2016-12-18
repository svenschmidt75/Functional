module ConstantTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Data.Monoid

import Lib
    ( Constant (..)
    )

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Constant Functor laws" $ do
                constantFunctorLawsProp
            prop "Constant Traversable laws" $ do
                printTestCase "Failure: " constantTraversableLawsProp

constantFunctorLawsProp :: Property
constantFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Constant String (Int, Int, Int)
    run $ quickBatch $ functor trigger

constantTraversableLawsProp :: Property
constantTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Constant String (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
