module StateSSSpec (spec) where

import Test.Hspec

import Lib ( State (..)
           , get
           , put
           , exec
           , eval
           , modify
           , state
           )

spec :: Spec
spec = do
    describe "State" $ do
        it "State s s" $ do
            (runState get "curryIsAmaze") `shouldBe` ("curryIsAmaze", "curryIsAmaze")
        it "State s ()" $ do
            (runState (put "blah") "woot") `shouldBe` ((), "blah")
        it "State exec - 1" $ do
            (exec (put "wilma") "daphne") `shouldBe` "wilma"
        it "State exec - 2" $ do
            (exec get "scooby papu") `shouldBe` "scooby papu"
        it "State eval - 1" $ do
            (eval get "bunnicula") `shouldBe` "bunnicula"
        it "State eval - 2" $ do
            (eval get "stake a bunny") `shouldBe` "stake a bunny"
        it "State modify - 1" $ do
            (runState (modify (+1)) 0) `shouldBe` ((), 1)
        it "State modify - 2" $ do
            (runState (modify (+1) >> modify (+1)) 0) `shouldBe` ((), 2)
        it "State state" $ do
            let s = state $ \s -> (s, s + 1)
            (runState s 0) `shouldBe` (0, 1)
