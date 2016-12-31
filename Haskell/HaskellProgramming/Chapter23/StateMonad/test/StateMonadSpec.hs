module StateMonadSpec (spec) where

import Test.Hspec

import Lib (State (..))

spec :: Spec
spec = do
    describe "State" $ do
        it "Functor 1" $ do
            let state = State $ \s -> (s + 1 :: Int, s)
            let applied = runState ((+1) <$> state) 1
            applied `shouldBe` (3, 1)
        it "Functor 2" $ do
            let state = State $ \s -> (0, s)
            let applied = runState ((+1) <$> state) 0
            applied `shouldBe` (1, 0)
        it "Applicative - pure" $ do
            let fa = State $ \s -> (s + 1 :: Int, s)
            let fab = (pure (+1)) :: State Int (Int -> Int)
            let applied = runState (fab <*> fa) 1
            applied `shouldBe` (3, 1)
        it "Applicative - <*>" $ do
            let fa = State $ \s -> (s + 3, s + 1 :: Int)
            let fab = State $ \s -> (\a -> a + 7, s + 4)
            let applied = runState (fab <*> fa) 1
            applied `shouldBe` (11, 6)
        it "Monad - >>=" $ do
            let fa = State $ \s -> (s + 1 :: Int, s + 3)
            let fab = \a -> State $ \s -> (a + 7, s + 4)
            let applied = runState (fa >>= fab) 1
            applied `shouldBe` (9, 8)
