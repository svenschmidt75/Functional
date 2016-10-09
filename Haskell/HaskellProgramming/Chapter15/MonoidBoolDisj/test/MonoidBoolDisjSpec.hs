module MonoidBoolDisjSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup
import Data.Monoid hiding ((<>))

import Lib
    ( BoolDisj (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Arbitrary BoolDisj where
    arbitrary = frequency [
                            (1, return $ BoolDisj True)
                          , (1, return $ BoolDisj False)
                          ]

type BoolConjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


spec :: Spec
spec = do
    describe "conidtions" $ do
        it "condition 1" $ do
            (BoolDisj True) `mappend` mempty `shouldBe` BoolDisj True
        it "condition 2" $ do
            mempty `mappend` (BoolDisj False) `shouldBe` BoolDisj False
    describe "BoolDisj Bool" $ do
        prop "verify associativety" $ do
            semigroupAssoc :: BoolConjAssoc
        prop "left identity" $ do
            monoidLeftIdentity :: BoolDisj -> Bool
        prop "right identity" $ do
            monoidRightIdentity :: BoolDisj -> Bool
