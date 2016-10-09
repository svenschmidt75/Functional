module MonoidBoolConjSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup
import Data.Monoid hiding ((<>))

import Lib
    ( BoolConj (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Semigroup m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Arbitrary BoolConj where
    arbitrary = frequency [
                            (1, return $ BoolConj True)
                          , (1, return $ BoolConj False)
                          ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


spec :: Spec
spec = do
    describe "conidtions" $ do
        it "condition 1" $ do
            (BoolConj True) `mappend` mempty `shouldBe` BoolConj True
        it "condition 2" $ do
            mempty `mappend` (BoolConj False) `shouldBe` BoolConj False
    describe "BoolConj Bool" $ do
        prop "verify associativety" $ do
            semigroupAssoc :: BoolConjAssoc
        prop "left identity" $ do
            monoidLeftIdentity :: BoolConj -> Bool
        prop "right identity" $ do
            monoidRightIdentity :: BoolConj -> Bool
