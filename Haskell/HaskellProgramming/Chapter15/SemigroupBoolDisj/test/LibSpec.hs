module LibSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Semigroup

import Lib
    ( BoolDisj (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

instance Arbitrary BoolDisj where
    arbitrary = frequency
                  [
                    (1, return $ BoolDisj True)
                  , (1, return $ BoolDisj False)
                  ]


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Semigroup Properties" $ do
    it "1st condition" $ do
      (BoolDisj True) <> (BoolDisj True) `shouldBe` BoolDisj True
    it "2nd condition" $ do
      (BoolDisj True) <> (BoolDisj False) `shouldBe` BoolDisj True

    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: BoolDisj) (b :: BoolDisj) (c :: BoolDisj)
