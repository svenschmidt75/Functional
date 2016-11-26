module ValidationApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Monadic (monadicIO, run)

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

data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
    deriving (Eq, Show)

spec :: Spec
spec = do
--    prop "List Applicative laws" $ do
--        validationFunctorLawsProp
    describe "test examples" $ do
        it "test 1" $ do
            let expected = Success 2 :: Validation String Int
            Success (+1) <*> Success 1 `shouldBe` expected
        it "test 2" $ do
            let expected = Failure [StackOverflow] :: Validation [Errors] Int
            Success (+1) <*> Failure [StackOverflow] `shouldBe` expected
        it "test 3" $ do
            let expected = Failure [StackOverflow] :: Validation [Errors] Int
            Failure [StackOverflow] <*> Success (+1) `shouldBe` expected
        it "test 4" $ do
            let expected = Failure [MooglesChewedWires, StackOverflow] :: Validation [Errors] Int
            Failure [MooglesChewedWires] <*> Failure [StackOverflow] `shouldBe` expected

validationFunctorLawsProp :: Property
validationFunctorLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: Validation String (Int, Int, Int)
    run $ quickBatch $ functor trigger
