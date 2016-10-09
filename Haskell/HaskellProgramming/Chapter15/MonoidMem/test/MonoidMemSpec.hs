module MonoidMemSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup
import Data.Monoid hiding ((<>))

import Lib
    ( Mem (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


type CombineAssoc = Combine Int (Sum Int) -> Combine Int (Sum Int) -> Combine Int (Sum Int) -> Bool


spec :: Spec
spec = do
    describe "conidtions" $ do
        it "condition 1" $ do
            let f = Combine $ \n -> Sum (n + 1)
            let result = unCombine (mappend f mempty) $ 1
            result `shouldBe` Sum 2

-- I need to figure out how to generate functions in QuickCheck (CoArbitrary)
{-
    describe "Combine" $ do
        prop "verify associativety" $ do
            semigroupAssoc :: CombineAssoc
        prop "left identity" $ do
            monoidLeftIdentity :: Combine Int (Sum Int) -> Bool
        prop "right identity" $ do
            monoidRightIdentity :: Combine Int (Sum Int) -> Bool
-}
