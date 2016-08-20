module LibSpec (spec) where

import Lib (solver)

import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "solver" $ do
        it "simple triangle" $ do
            let edges = [
                          Edge (Vertex 1) (Vertex 2) 0,
                          Edge (Vertex 2) (Vertex 3) 0,
                          Edge (Vertex 3) (Vertex 1) 0
                        ]
            let graph = Graph [Vertex 1, Vertex 2, Vertex 3] edges
            let expected = [[Vertex 1, Vertex 2, Vertex 3]]
            solver graph `shouldBe` expected
