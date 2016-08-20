module Graph
    ( Vertex (..)
    , Edge (..)
    , Graph (..)
    , hasVertex
    , addVertex
    , hasEdge
    , addEdge
    , adjacent
    , neighbors
    ) where

-- Each vertex has a unique index
data Vertex = Vertex Int
        deriving (Eq, Show)

type Color = Int

-- An edge connects two vertices
data Edge = Edge Vertex Vertex Color
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
hasEdge (Edge v1 v2 _) (Graph _ edges) = hasEdge' edges
                                           where
                                             hasEdge' [] = False
                                             hasEdge' (Edge x1 x2 _ : es)
                                               | x1 == v1 && x2 == v2 = True
                                               | x1 == v2 && x2 == v1 = True
                                               | otherwise            = hasEdge' es

addEdge :: Edge -> Graph -> Graph
addEdge e@(Edge v1 v2 _) g@(Graph vs es)
    | hasEdge e g             = g
    | hasVertex v1 g == False = error $ "addEdge: Vertex " ++ show v1 ++ " not in graph"
    | hasVertex v2 g == False = error $ "addEdge: Vertex " ++ show v2 ++ " not in graph"
    | otherwise               = Graph vs (e:es)

--adjacent(G, x, y): tests whether there is an edge from the vertices x to y;
adjacent :: Vertex -> Vertex -> Graph -> Bool
adjacent v1 v2 = hasEdge (Edge v1 v2 0)

--neighbors(G, x): lists all vertices y such that there is an edge from the vertices x to y;
neighbors :: Vertex -> Graph -> [Vertex]
neighbors vertex (Graph _ edges) = neighbors' edges []
                                 where
                                   neighbors' [] vs = vs
                                   neighbors' (Edge v1 v2 _ : es) vs
                                       | v1 == vertex = v2 : neighbors' es vs
                                       | v2 == vertex = v1 : neighbors' es vs
                                       | otherwise    =      neighbors' es vs
