module InsertSpec (spec) where


import Lib
       ( BinaryTree (..)
       , insert'
       , mapTree
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "BinaryTree Insert" $ do
        it "create from empty tree" $ do
            (insert' 1 Leaf) `shouldBe` (Node (Leaf) 1 (Leaf))

        it "add smaller" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node (Node Leaf 1 Leaf) 10 Leaf
            (insert' 1 bt) `shouldBe` expected

        it "add same" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node Leaf 10 Leaf
            (insert' 10 bt) `shouldBe` expected

        it "add bigger" $ do
            let bt = Node Leaf 10 Leaf
            let expected = Node Leaf 10 (Node Leaf 11 Leaf)
            (insert' 11 bt) `shouldBe` expected

    describe "BinaryTree Map" $ do
        it "map on empty tree" $ do
            (mapTree (+1) Leaf) `shouldBe` Leaf

        it "map on non-empty tree" $ do
            let bt = Node Leaf 10 (Node Leaf 6 Leaf)
            let expected = Node Leaf 11 (Node Leaf 7 Leaf)
            (mapTree (+1) bt) `shouldBe` expected

        it "map on non-empty tree 2" $ do
            let bt = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
            let expected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
            (mapTree (+1) bt) `shouldBe` expected
