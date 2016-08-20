module Lib
    ( solver
    ) where

import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       , neighbors
       , isEdgeColored
       )

import Data.Maybe (isNothing)


getUncoloredEdges :: Vertex -> Graph -> [Edge]
getUncoloredEdges v g =
                    let neighboringVertices = neighbors v g
                        edges               = [Edge v y 0 | y <- neighboringVertices]
                        nonColoredEdges     = filter (\(Edge v1 v2 _) -> isNothing $ isEdgeColored v1 v2 g) edges
                    in nonColoredEdges

solver :: Graph -> [[Vertex]]
solver g = do
       let (Graph vs es) = g
       return []
