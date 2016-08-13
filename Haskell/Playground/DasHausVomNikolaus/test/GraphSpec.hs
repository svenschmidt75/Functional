module GraphSpec (spec) where


import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       , hasVertex
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "Vertex" $ do
        it "hasVertex" $ do
            let graph = Graph [Vertex 1] [Edge (Vertex 1) (Vertex 2)]
            hasVertex (Vertex 2) graph `shouldBe` False
