module HuttonsRazorSpec (spec) where


import Lib
       ( Expr (..)
       , eval
       , printExpr
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "Evaluator" $ do
        it "Simple Addition" $ do
            (eval (Add (Lit 1) (Lit 9001))) `shouldBe` 9002

    describe "Pretty Printer" $ do
        it "Simple Addition" $ do
            (printExpr (Add (Lit 1) (Lit 9001))) `shouldBe` "1 + 9001"

        it "Nested Addition" $ do
            let a1 = Add (Lit 9001) (Lit 1)
            let a2 = Add a1 (Lit 20001)
            let a3 = Add (Lit 1) a2
            (printExpr a3) `shouldBe` "1 + 9001 + 1 + 20001"
