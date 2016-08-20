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
    , getEdge
    , isEdgeColored
    , colorEdge
    ) where

import Helper

-- Each vertex has a unique index
data Vertex = Vertex Int
        deriving (Eq, Show)

type Color = Int

-- An edge connects two vertices
data Edge = Edge Vertex Vertex Color
        deriving (Show)

-- ignore edge color when comparing edges
instance Eq Edge where
    (Edge v1 v2 _) == (Edge y1 y2 _) = v1 == y1 && v2 == y2

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
                                             hasEdge' []              = False
                                             hasEdge' (Edge x1 x2 _ : es)
                                               | x1 == v1 && x2 == v2 = True
                                               | x1 == v2 && x2 == v1 = True
                                               | otherwise            = hasEdge' es

-- undirected edges
getEdge :: Edge -> Graph -> Maybe Edge
getEdge (Edge v1 v2 _) (Graph _ edges) = getEdge' edges
                                           where
                                             getEdge' []              = Nothing
                                             getEdge' (e@(Edge x1 x2 _):es)
                                               | x1 == v1 && x2 == v2 = Just e
                                               | x1 == v2 && x2 == v1 = Just e
                                               | otherwise            = getEdge' es

addEdge :: Edge -> Graph -> Graph
addEdge e@(Edge v1 v2 _) g@(Graph vs es)
    | hasEdge e g             = g
    | hasVertex v1 g == False = error $ "addEdge: Vertex " ++ show v1 ++ " not in graph"
    | hasVertex v2 g == False = error $ "addEdge: Vertex " ++ show v2 ++ " not in graph"
    | otherwise               = Graph vs (e:es)

isEdgeColored :: Vertex -> Vertex -> Graph -> Maybe Bool
isEdgeColored v1 v2 g
    | hasEdge (Edge v1 v2 0) g = do
                               e <- getEdge (Edge v1 v2 0) g
                               let (Edge _ _ c) = e
                               return (c /= 0)
    | otherwise                = Nothing

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

colorEdge :: Edge -> Color -> Graph -> Graph
colorEdge e@(Edge v1 v2 _) c (Graph vs es) = Graph vs $ replace e (Edge v1 v2 c) es
