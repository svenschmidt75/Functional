module OOPSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

--import Lib
--    ( Identity (..)
--    )

spec :: Spec
spec = do
    describe "dummy" $
        it "dummy" $
            True `shouldBe` True
