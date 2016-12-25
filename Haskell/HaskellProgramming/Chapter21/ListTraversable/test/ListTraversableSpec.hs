module ListTraversableSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

import Data.Monoid

import Lib
    ( List (..)
    )

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [(1, return Nil), (1, Cons <$> arbitrary <*> return Nil)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "verify laws" $ do
        modifyMaxSuccess (const 1) $ do
            prop "List Functor laws" $ do
                listFunctorLawsProp
            prop "List Applicative laws" $ do
                listApplicativeLawsProp
            prop "List Traversable laws" $ do
                listTraversableLawsProp

listFunctorLawsProp :: Property
listFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: List (Int, Int, Int)
    run $ quickBatch $ functor trigger

listApplicativeLawsProp :: Property
listApplicativeLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: List (Int, Int, Int)
    run $ quickBatch $ applicative trigger

listTraversableLawsProp :: Property
listTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: List (Int, Int, [Int])
    run $ quickBatch $ traversable trigger
