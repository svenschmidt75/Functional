module STraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( S (..)
    )

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (S n a) where
   arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (S n a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "S' Functor laws" $ do
                sFunctorLawsProp
--            prop "S' Traversable laws" $ do
--                sthreeTraversableLawsProp

sFunctorLawsProp :: Property
sFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: S Maybe (Int, Int, Int)
    run $ quickBatch $ functor trigger

-- sthreeTraversableLawsProp :: Property
-- sthreeTraversableLawsProp = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: S' Int (Int, Int, [Int])
--     run $ quickBatch $ traversable trigger
