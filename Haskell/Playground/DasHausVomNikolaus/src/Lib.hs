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


solver :: Graph -> [[Vertex]]
solver g = do
       let (Graph vs es) = g
       return []
