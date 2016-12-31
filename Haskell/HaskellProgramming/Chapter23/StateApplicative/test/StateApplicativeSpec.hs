module StateApplicativeSpec (spec) where

import Test.Hspec

import Lib (State (..))

spec :: Spec
spec = do
    describe "State" $ do
        it "Functor" $ do
            let state = State $ \s -> (s, s + 1 :: Int)
            let applied = runState ((+1) <$> state) 1
            applied `shouldBe` (1, 3)
        it "Applicative - pure" $ do
            let fa = State $ \s -> (s, s + 1 :: Int)
            let fab = (pure (+1)) :: State Int (Int -> Int)
            let applied = runState (fab <*> fa) 1
            applied `shouldBe` (1, 3)
        it "Applicative - <*>" $ do
            let fa = State $ \s -> (s + 3, s + 1 :: Int)
            let fab = State $ \s -> (s + 4, \a -> a + 7)
            let applied = runState (fab <*> fa) 1
            applied `shouldBe` (8, 9)
