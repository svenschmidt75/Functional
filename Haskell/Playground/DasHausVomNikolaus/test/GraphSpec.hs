module GraphSpec (spec) where


import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       , hasVertex
       , addVertex
       , hasEdge
       , addEdge
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "Vertex" $ do
        it "hasVertex - does not yet exist" $ do
            let graph = Graph [Vertex 1] [Edge (Vertex 1) (Vertex 2)]
            hasVertex (Vertex 2) graph `shouldBe` False

        it "hasVertex - exists" $ do
            let graph = Graph [Vertex 1] [Edge (Vertex 1) (Vertex 2)]
            hasVertex (Vertex 1) graph `shouldBe` True

        it "addVertex - does not yet exist" $ do
            let isGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            let expectedGraph = Graph [Vertex 3, Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            addVertex (Vertex 3) isGraph `shouldBe` expectedGraph

        it "addVertex - exists" $ do
            let isGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            let expectedGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            addVertex (Vertex 1) isGraph `shouldBe` expectedGraph
            addVertex (Vertex 2) isGraph `shouldBe` expectedGraph


    describe "Edge" $ do
        it "hasEdge - does not yet exist" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            hasEdge (Edge (Vertex 3) (Vertex 2)) graph `shouldBe` False

        it "hasEdge - exist" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            hasEdge (Edge (Vertex 1) (Vertex 2)) graph `shouldBe` True

        it "hasEdge - exist, undirected" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            hasEdge (Edge (Vertex 2) (Vertex 1)) graph `shouldBe` True

        it "addEdge - does not yet exist" $ do
            let isGraph = Graph [Vertex 1, Vertex 2] []
            let expectedGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            addEdge (Edge (Vertex 1) (Vertex 2)) isGraph `shouldBe` expectedGraph

        it "addEdge - exists" $ do
            let isGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            let expectedGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            addEdge (Edge (Vertex 1) (Vertex 2)) isGraph `shouldBe` expectedGraph

        it "addEdge - exists, undirected" $ do
            let isGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            let expectedGraph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            addEdge (Edge (Vertex 2) (Vertex 1)) isGraph `shouldBe` expectedGraph
