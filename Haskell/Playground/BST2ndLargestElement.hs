import Data.List

data Tree a = Node (Tree a) a (Tree a)
            | Leaf
    deriving (Show)

flatten :: Tree a -> [a]
flatten Leaf         = []
flatten (Node l a r) = flatten l ++ [a] ++ flatten r

main :: IO ()
main = do
    let tree = Node (Node Leaf 2 Leaf) 5 (Node (Node Leaf 4 Leaf) 3 (Node Leaf 7 Leaf))
    print tree
    let flattened_tree = reverse . sort $ flatten tree
    print flattened_tree
    putStrLn "2nd largest element: "
    print $ flattened_tree !! 1