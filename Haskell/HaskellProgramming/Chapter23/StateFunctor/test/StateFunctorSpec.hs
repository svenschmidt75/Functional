module StateFunctorSpec (spec) where

import Test.Hspec

import Lib (State (..))

spec :: Spec
spec = do
    describe "State" $ do
        it "Functor" $ do
            let state = State $ \s -> (s, s + 1 :: Int)
            let applied = runState ((+1) <$> state) 1
            applied `shouldBe` (1, 3)
