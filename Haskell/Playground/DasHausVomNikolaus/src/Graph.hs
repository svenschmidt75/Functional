module Graph
    ( Vertex (..)
    , Edge (..)
    , Graph (..)
    , hasVertex
    ) where

-- Each vertex has a unique index
data Vertex = Vertex Int

-- An edge connects two vertices
data Edge = Edge Vertex Vertex

data Graph = Graph [Vertex] [Edge]

hasVertex :: Vertex -> Graph -> Bool
hasVertex (Vertex i) (Graph vertices _) = hasVertex' vertices
                                          where
                                            hasVertex' [] = False
                                            hasVertex' (Vertex x:xs)
                                              | x == i    = True
                                              | otherwise = hasVertex' xs


--addVertex :: Vertex -> Graph -> Graph
--addVertex v g