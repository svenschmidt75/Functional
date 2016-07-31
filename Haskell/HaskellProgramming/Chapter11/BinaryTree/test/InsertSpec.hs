module InsertSpec (spec) where


import Lib
       ( BinaryTree (..)
       , insert'
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "BinaryTree" $ do
        it "create from empty tree" $ do
            (insert' 1 Leaf) `shouldBe` (Node (Leaf) 1 (Leaf))

        it "add smaller" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node (Node Leaf 1 Leaf) 10 Leaf
            (insert' 1 bt) `shouldBe` expected

        it "add same" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node (Node Leaf 1 Leaf) 10 Leaf
            (insert' 1 bt) `shouldBe` expected

        it "add bigger" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node Leaf 10 (Node Leaf 11 Leaf)
            (insert' 11 bt) `shouldBe` expected
