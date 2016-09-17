module Main where

import Data.Semigroup
import Test.QuickCheck

import Lib


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return $ Sum a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: TwoAssoc String (Sum Int))
