module Main where

import Data.Monoid

import Test.QuickCheck

import Lib
     ( firstMappend
     , FstId
     , FirstMappend
     , First' (..)
     )

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a



{-
instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = arbitrarySum

arbitrarySum :: Arbitrary a => Gen (Sum a)
arbitrarySum = do
    a <- arbitrary
    return $ Sum a


instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        frequency [
                    (1, return $ Only a)
                  , (1, return Nada)
                  ]
-}

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        return $ First' $ Just a


main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
