module InsertSpec (spec) where


import Lib
       ( BinaryTree (..)
       , insert'
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "Parser" $ do
        it "returns the unit value on empty input" $ do
            (insert' 1 Leaf) `shouldBe` (Node (Leaf) 1 (Leaf))
