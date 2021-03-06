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
       , allEdgesColored
       )

import Debug.Trace
import Data.List (nub)


solver :: Graph -> [[Vertex]]
solver g =
    let (Graph vs _)         = g
        -- find all paths
        allPaths             = concatMap (\v -> step [] v g) vs
        -- only those whose edges have all been visited are solutions
        pathsAllEdgesColored = map snd $ filter (\(graph, _) -> allEdgesColored graph) allPaths
        -- remove duplicates
        paths                = nub pathsAllEdgesColored
    in paths

step :: [Vertex] -> Vertex -> Graph -> [(Graph, [Vertex])]
--step currentPath v1 g | trace ("step: v1=" ++ show v1 ++ " currentPath=" ++ show currentPath ++ " graph=" ++ show g) False = undefined
step currentPath v1 g =
    let vs   = neighbors v1 g
        path = concatMap startSolve' vs
    in path
  where
    startSolve' :: Vertex -> [(Graph, [Vertex])]
--    startSolve' v2 | trace ("startSolve': v2=" ++ show v2) False = undefined
    startSolve' v2 =
        if isEdgeColored v1 v2 g
            then [(g, v1 : currentPath)]
            else step (v1 : currentPath) v2 coloredGraph
          where
            coloredGraph = colorEdge (Edge v1 v2 0) 1 g
