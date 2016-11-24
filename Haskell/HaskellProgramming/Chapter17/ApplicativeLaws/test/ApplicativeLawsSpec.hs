module ApplicativeLawsSpec (spec) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Bull (..)
    )

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo) ]

instance EqProp Bull where (=-=) = eq

spec :: Spec
spec = do
    prop "Monoid laws" $ do
        prop5

prop5 :: Property
prop5 = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    run $ quickBatch (monoid Twoo)
