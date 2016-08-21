module Lib
    ( solver
    ) where

import Graph
       ( Vertex (..)
       , Edge (..)
       , Graph (..)
       , neighbors
       , isEdgeColored
       , colorEdge
       )

import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.List (nub)


solver :: Graph -> [[Vertex]]
solver g =
       let (Graph vs _) = g in
       let results = concatMap (\v -> filter (\x -> length x == 8) $ nub $ step [] v g) vs
       in results

step :: [Vertex] -> Vertex -> Graph -> [[Vertex]]
step currentPath v1 g | trace ("step: v1=" ++ show v1 ++ " currentPath=" ++ show currentPath ++ " graph=" ++ show g) False = undefined
step currentPath v1 g =
            let vs   = neighbors v1 g
                path = concatMap startSolve' vs
            in path
            where
                startSolve' v2 | trace ("startSolve': v2=" ++ show v2) False = undefined
                startSolve' v2 =
                            case fromMaybe True $ isEdgeColored v1 v2 g of
                              True -> [currentPath]
                              False -> step (v1 : currentPath) v2 coloredGraph
                                        where
                                          coloredGraph = colorEdge (Edge v1 v2 0) 1 g
