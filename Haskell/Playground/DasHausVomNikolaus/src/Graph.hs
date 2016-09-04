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
    , getUncoloredEdges
    , allEdgesColored
    ) where

import Debug.Trace

import Helper


-- Each vertex has a unique index
data Vertex = Vertex Int
        deriving (Eq, Show)

-- edge color
-- 0: edge has not been visited
-- 1: edge has been visited
type Color = Int

-- An edge connects two vertices
data Edge = Edge Vertex Vertex Color
        deriving (Eq, Show)

-- should this type be UndirectedGraph?
data Graph = Graph [Vertex] [Edge]
        deriving (Eq, Show)


hasVertex :: Vertex -> Graph -> Bool
hasVertex (Vertex v) (Graph vertices _) =
    hasVertex' vertices
  where
    hasVertex' [] = False
    hasVertex' (Vertex x:xs)
        | x == v    = True
        | otherwise = hasVertex' xs

addVertex :: Vertex -> Graph -> Graph
addVertex v g@(Graph vs es)
    | hasVertex v g = g
    | otherwise     = Graph (v:vs) es

-- undirected edges
hasEdge :: Edge -> Graph -> Bool
hasEdge (Edge v1 v2 _) (Graph _ edges) =
    hasEdge' edges
  where
    hasEdge' []              = False
    hasEdge' (Edge x1 x2 _ : es)
        | x1 == v1 && x2 == v2 = True
        | x1 == v2 && x2 == v1 = True
        | otherwise            = hasEdge' es

-- undirected edges
getEdge :: Edge -> Graph -> Edge
getEdge (Edge v1 v2 _) (Graph _ edges) =
    getEdge' edges
  where
    getEdge' []              = error $ "getEdge: Edge (" ++ show v1 ++ ", " ++ show v2 ++ ") not in graph"
    getEdge' (e@(Edge x1 x2 _):es)
        | x1 == v1 && x2 == v2 = e
        | x1 == v2 && x2 == v1 = e
        | otherwise            = getEdge' es

addEdge :: Edge -> Graph -> Graph
addEdge e@(Edge v1 v2 _) g@(Graph vs es)
    | hasEdge e g             = g
    | hasVertex v1 g == False = error $ "addEdge: Vertex " ++ show v1 ++ " not in graph"
    | hasVertex v2 g == False = error $ "addEdge: Vertex " ++ show v2 ++ " not in graph"
    | otherwise               = Graph vs (e:es)

--adjacent(G, x, y): tests whether there is an edge from vertices x to y;
adjacent :: Vertex -> Vertex -> Graph -> Bool
adjacent v1 v2 = hasEdge (Edge v1 v2 0)

--neighbors(G, x): lists all vertices y such that there is an edge from the vertices x to y;
neighbors :: Vertex -> Graph -> [Vertex]
neighbors vertex (Graph _ edges) =
    neighbors' edges []
  where
    neighbors' [] vs = vs
    neighbors' (Edge v1 v2 _ : es) vs
        | v1 == vertex = v2 : neighbors' es vs
        | v2 == vertex = v1 : neighbors' es vs
        | otherwise    =      neighbors' es vs

-- edges are undirected, so only compare vertices for equality
edgeComparator :: Edge -> Edge -> Bool
edgeComparator (Edge v1 v2 _) (Edge y1 y2 _) =
       v1 == y1 && v2 == y2
    || v1 == y2 && v2 == y1

colorEdge :: Edge -> Color -> Graph -> Graph
colorEdge e@(Edge v1 v2 _) c (Graph vs es) =
    Graph vs $ replace' edgeComparator e (Edge v1 v2 c) es

isEdgeColored :: Vertex -> Vertex -> Graph -> Bool
isEdgeColored v1 v2 g
    | hasEdge (Edge v1 v2 0) g =
        let (Edge _ _ c) = getEdge (Edge v1 v2 0) g
        in c /= 0
    | otherwise                = error $ "getEdge: Edge (" ++ show v1 ++ ", " ++ show v2 ++ ") not in graph"

getUncoloredEdges :: Vertex -> Graph -> [Edge]
getUncoloredEdges v g =
    let neighboringVertices = neighbors v g
        edges               = [Edge v y 0 | y <- neighboringVertices]
        nonColoredEdges     = filter (\(Edge v1 v2 _) -> not $ isEdgeColored v1 v2 g) edges
    in nonColoredEdges

allEdgesColored :: Graph -> Bool
allEdgesColored g@(Graph _ es) =
    all (\(Edge v1 v2 _) -> isEdgeColored v1 v2 g) es
