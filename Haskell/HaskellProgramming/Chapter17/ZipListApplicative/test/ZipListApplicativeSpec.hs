module ZipListApplicativeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( List (..)
    , ZipList' (..)
    )


take' :: Int -> List a -> List a
take' = undefined

instance Eq a => EqProp (List a) where
    (=-=) = eq

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
        it "test 3" $ do
            let functions = Nil
            let values = Cons 1 (Cons 2 Nil)
            let result = functions <*> values
            result `shouldBe` (Nil :: List Int)
        it "test 4" $ do
            let functions = Cons (+1) Nil
            let values = Nil
            let result = functions <*> values
            result `shouldBe` (Nil :: List Int)
--        prop "List Applicative laws" $ do
--            prop5
    describe "ZipList' Applicative Tests" $ do
        it "test 1" $ do
            let functions = ZipList' $ Cons (+9) (Cons (*2) (Cons (+8) Nil))
            let values = ZipList' $ Cons 1 (Cons 2 (Cons 3 Nil))
            let result = functions <*> values
            result `shouldBe` ZipList' (Cons 10 (Cons 4 (Cons 11 Nil)))
--        it "with infinite list" $ do
--            let functions = ZipList' [(+9), (*2), (+8)]
--            let values = ZipList' (repeat 1)
--            let result = functions <*> values
--            result `shouldBe` [10,2,9]

--prop5 :: Property
--prop5 = monadicIO $ do
--    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--    let trigger = undefined :: [(List Int, List Int, List Int)]
--   run $ quickBatch $ applicative trigger
