module Main where

import Lib (solver)

import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       )

main :: IO ()
main = do
        let edges = [
                      Edge (Vertex 1) (Vertex 2) 0,
                      Edge (Vertex 2) (Vertex 3) 0,
                      Edge (Vertex 3) (Vertex 1) 0
                    ]
        let graph = Graph [Vertex 1, Vertex 2, Vertex 3] edges
        print $ solver graph
