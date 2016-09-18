module LibSpec
    ( main
    , spec
    ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Semigroup

import Lib
    ( Or (Fst, Snd)
    )


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance (Num a, Num b) => Arbitrary (Or a b) where
    arbitrary = frequency
                  [
                    (1, return $ Fst 1)
                  , (1, return $ Snd 2)
                  ]


-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Semigroup Properties" $ do
    it "1st condition" $ do
      (Fst 1) <> (Snd 2) `shouldBe` ((Snd 2) :: Or Int Int)
    it "2nd condition" $ do
      (Fst 1) <> (Fst 2) `shouldBe` ((Fst 2) :: Or Int Int)
    it "3rd condition" $ do
      (Snd 1) <> (Fst 2) `shouldBe` ((Snd 1) :: Or Int Int)
    it "4th condition" $ do
      (Snd 1) <> (Snd 2) `shouldBe` ((Snd 1) :: Or Int Int)

    describe "Semigroup Associativity" $ do
        prop "verify associativety" $
            \a b c -> semigroupAssoc (a :: Or Int Int) (b :: Or Int Int) (c :: Or Int Int)
