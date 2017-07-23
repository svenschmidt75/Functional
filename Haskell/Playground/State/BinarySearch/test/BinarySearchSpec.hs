{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
module BinarySearchSpec (spec) where

import Prelude hiding (min, max)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Control.Monad.Trans.State.Lazy ( runState
                                      , evalState)


import Lib
    ( binarySearch
    )

spec :: Spec
spec = do
    describe "Binary Search" $ do
        it "empty array" $
            fst (runState (binarySearch [] 1) 0) `shouldBe` False
        it "one-element array" $ do
            let found = evalState (binarySearch [0] 1) 0
            found `shouldBe` False
        it "element at index 0" $ do
            let (found, n) = runState (binarySearch [1, 2, 3, 4, 5] 1) 0
            found `shouldBe` True
            n `shouldSatisfy` (< 3)
        it "element at last index" $ do
            let (found, n) = runState (binarySearch [1, 2, 3, 4, 5] 5) 0
            found `shouldBe` True
            n `shouldSatisfy` (< 3)
        it "element spmewhere" $ do
            let (found, n) = runState (binarySearch [1, 2, 3, 4, 5, 9, 18, 465, 3645] 465) 0
            found `shouldBe` True
            n `shouldSatisfy` (< 4)
        it "element too small" $ do
            let found = evalState (binarySearch [1, 2, 3, 4, 5, 9, 18, 465, 3645] (-1)) 0
            found `shouldBe` False
