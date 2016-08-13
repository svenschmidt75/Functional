module GraphSpec (spec) where


import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       , hasVertex
       , addVertex
       , hasEdge
       , addEdge
       , adjacent
       , neighbors
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

        it "adjacent - edge does not exist" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            adjacent (Vertex 3) (Vertex 2) graph `shouldBe` False

        it "adjacent - exists" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            adjacent (Vertex 1) (Vertex 2) graph `shouldBe` True

        it "adjacent - exists, undirected" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            adjacent (Vertex 2) (Vertex 1) graph `shouldBe` True

    describe "neighbors" $ do
        it "vertex has no neighbors" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            neighbors (Vertex 3) graph `shouldBe` []

        it "vertex has one neighbor" $ do
            let graph = Graph [Vertex 1, Vertex 2] [Edge (Vertex 1) (Vertex 2)]
            neighbors (Vertex 1) graph `shouldBe` [Vertex 2]

        it "vertex has neighbors" $ do
            let vertices = [Vertex 1, Vertex 2, Vertex 3, Vertex 4]
            let edges = [
                            Edge (Vertex 1) (Vertex 2)
                          , Edge (Vertex 2) (Vertex 3)
                          , Edge (Vertex 3) (Vertex 4)
                          , Edge (Vertex 4) (Vertex 1)
                          , Edge (Vertex 4) (Vertex 2)
                          , Edge (Vertex 1) (Vertex 3)
                        ]
            let graph = Graph vertices edges
            neighbors (Vertex 1) graph `shouldBe` [Vertex 2, Vertex 4, Vertex 3]
            neighbors (Vertex 2) graph `shouldBe` [Vertex 1, Vertex 3, Vertex 4]
            neighbors (Vertex 3) graph `shouldBe` [Vertex 2, Vertex 4, Vertex 1]
            neighbors (Vertex 4) graph `shouldBe` [Vertex 3, Vertex 1, Vertex 2]
