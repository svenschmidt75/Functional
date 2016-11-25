module ZipListApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Checkers
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( List (..)
    , ZipList' (..)
    )


take' :: Int -> List a -> List a
take' = undefined

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
      where xs' = let (ZipList' l) = xs
                  in take' 3000 l
            ys' = let (ZipList' l) = ys
                  in take' 3000 l

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        frequency [(1, return $ Cons a Nil)
                 , (1, return $ Nil)]

spec :: Spec
spec = do
    describe "List Applicative Tests" $ do
        it "test 1" $ do
            let functions = Cons (+1) Nil
            let values = Cons 1 Nil
            let result = functions <*> values
            result `shouldBe` (Cons 2 Nil)
        it "test 2" $ do
            let functions = Cons (+1) (Cons (*2) Nil)
            let values = Cons 1 (Cons 2 Nil)
            let result = functions <*> values
            result `shouldBe` (Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil))))
