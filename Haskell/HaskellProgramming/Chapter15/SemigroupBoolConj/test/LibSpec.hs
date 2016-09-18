module LibSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Semigroup

import Lib
    ( BoolConj (..)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary BoolConj where
    arbitrary = frequency
                  [
                    (1, return $ BoolConj True)
                  , (1, return $ BoolConj False)
                  ]


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Semigroup Properties" $ do
    it "1st condition" $ do
      (BoolConj True) <> (BoolConj True) `shouldBe` BoolConj True
    it "2nd condition" $ do
      (BoolConj True) <> (BoolConj False) `shouldBe` BoolConj False

    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: BoolConj) (b :: BoolConj) (c :: BoolConj)
