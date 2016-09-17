module Main where

import Data.Semigroup
import Test.QuickCheck

import Lib


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type IdentityAssoc a = Identity a -> Identity a -> Identity a -> Bool


main :: IO ()
main =
    quickCheck (semigroupAssoc :: IdentityAssoc String)
