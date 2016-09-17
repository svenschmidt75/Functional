module Main where

import Data.Semigroup
import Test.QuickCheck

import Lib


instance Arbitrary Trivial where
    arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: TrivialAssoc)
