module Main where

import Data.Monoid

import Test.QuickCheck

import Lib
     ( FstId
     , FirstMappend
     , First' (..)
     )

{- The point here is to define a new monoid instance for Maybe a.
   We have to put Maybe a in a newtype, because there can be only
   one monoid instance per type.
-}

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = do
        a <- arbitrary
        frequency [
                    (1, return $ First' $ Just a)
                  , (1, return $ First' Nothing)
                  ]

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
