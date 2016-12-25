module TreeTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Lib
    ( Tree (..)
    )

instance (Arbitrary a, Arbitrary (n a)) => Arbitrary (Tree n a) where
   arbitrary = Tree <$> arbitrary <*> arbitrary

instance (Eq a, Eq (n a)) => EqProp (Tree n a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Tree Functor laws" $ do
                sFunctorLawsProp
            prop "Tree Traversable laws" $ do
                sTraversableLawsProp

sFunctorLawsProp :: Property
sFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Tree Maybe (Int, Int, Int)
    run $ quickBatch $ functor trigger

sTraversableLawsProp :: Property
sTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Tree Maybe (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
