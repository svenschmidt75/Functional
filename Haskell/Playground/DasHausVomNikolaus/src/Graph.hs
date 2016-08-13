module Graph
    ( Vertex (..)
    , Edge (..)
    , Graph (..)
    , hasVertex
    , addVertex
    , hasEdge
    , addEdge
    ) where

-- Each vertex has a unique index
data Vertex = Vertex Int
        deriving (Eq, Show)

-- An edge connects two vertices
data Edge = Edge Vertex Vertex
        deriving (Eq, Show)

-- should this type be UndirectedGraph?
data Graph = Graph [Vertex] [Edge]
        deriving (Eq, Show)

hasVertex :: Vertex -> Graph -> Bool
hasVertex (Vertex i) (Graph vertices _) = hasVertex' vertices
                                          where
                                            hasVertex' [] = False
                                            hasVertex' (Vertex x:xs)
                                              | x == i    = True
                                              | otherwise = hasVertex' xs

addVertex :: Vertex -> Graph -> Graph
addVertex v g@(Graph vs es)
    | hasVertex v g = g
    | otherwise     = Graph (v:vs) es

-- undirected edges
hasEdge :: Edge -> Graph -> Bool
hasEdge (Edge v1 v2) (Graph _ edges) = hasEdge' edges
                                          where
                                            hasEdge' [] = False
                                            hasEdge' (Edge x1 x2:es)
                                              | x1 == v1 && x2 == v2 = True
                                              | x1 == v2 && x2 == v1 = True
                                              | otherwise            = hasEdge' es

addEdge :: Edge -> Graph -> Graph
addEdge e@(Edge v1 v2) g@(Graph vs es)
    | hasEdge e g             = g
    | hasVertex v1 g == False = error $ "addEdge: Vertex " ++ show v1 ++ " noy in graph"
    | hasVertex v2 g == False = error $ "addEdge: Vertex " ++ show v2 ++ " noy in graph"
    | otherwise               = Graph vs (e:es)
