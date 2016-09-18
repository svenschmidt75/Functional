module LibSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Semigroup

import Lib
    ( Combine (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)



-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Semigroup Properties" $ do
        let f = Combine $ \n -> Sum (n + 1)
        let g = Combine $ \n -> Sum (n - 1)
        it "1st condition" $ do
            (unCombine (f <> g) $ 0) `shouldBe` (Sum 0)
        it "2nd condition" $ do
            (unCombine (f <> g) $ 1) `shouldBe` (Sum 2)
        it "3rd condition" $ do
            (unCombine (f <> f) $ 1) `shouldBe` (Sum 4)
        it "4th condition" $ do
            (unCombine (g <> f) $ 1) `shouldBe` (Sum 2)

{-
    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: BoolConj) (b :: BoolConj) (c :: BoolConj)
-}