module ReaderApplicativeSpec (spec) where

import Test.Hspec

import Lib

spec :: Spec
spec = do
    describe "(->) r Functor" $ do
        it "simple" $ do
            let r = (Reader $ \a -> a + 1) :: Reader Int Int
            runReader ((+1) <$> r) 1 `shouldBe` 3
    describe "(->) r Applicative" $ do
        it "pure" $ do
            let r = pure "Test"
            runReader r 1 `shouldBe` "Test"
        it "<*>" $ do
            let rab = Reader $ \r -> (\a -> a + r)
            let ra = (Reader $ \r -> r + 1) :: Reader Int Int
            runReader (rab <*> ra) 7 `shouldBe` 15
