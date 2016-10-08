module ValidationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Semigroup

import Lib
    ( Validation' (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
    arbitrary = do
                    a <- arbitrary
                    b <- arbitrary
                    frequency [
                                (1, return $ Failure' a)
                              , (1, return $ Success' b)
                              ]

spec :: Spec
spec = do
    describe "Semigroup Properties" $ do
        it "1st condition" $ do
            let expr1 = Failure' "Fail 1"
            let expr2 = Failure' "Fail 2"
            let expected = (Failure' "Fail 1Fail 2" :: Validation' String Int)
            expr1 <> expr2 `shouldBe` expected

    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: Validation' String Int) (b :: Validation' String Int) (c :: Validation' String Int)
