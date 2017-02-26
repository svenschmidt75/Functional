module CoArbitrarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib

-- instance [safe] (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)


spec :: Spec
spec = return ()