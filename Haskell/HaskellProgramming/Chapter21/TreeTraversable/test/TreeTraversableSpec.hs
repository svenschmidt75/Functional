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

instance Arbitrary a => Arbitrary (Tree a) where
   arbitrary = frequency
               [ (1, return Empty)
               , (1, Leaf <$> arbitrary)
               , (1, Node <$> (Leaf <$> arbitrary) <*> arbitrary <*> (Leaf <$> arbitrary))
               ]

instance Eq a => EqProp (Tree a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "Tree Functor laws" $ do
                treeFunctorLawsProp
            prop "Tree Traversable laws" $ do
                treeTraversableLawsProp

treeFunctorLawsProp :: Property
treeFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Tree (Int, Int, Int)
    run $ quickBatch $ functor trigger

treeTraversableLawsProp :: Property
treeTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Tree (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
