data Tree = Leaf Int
          | Node Tree Int Tree
          | Node2 Int Tree
          | Node3 Tree Int
  deriving Show

tree :: Tree
--tree = Node (Leaf 0) 1 (Node (Node (Leaf 2) 5 (Node (Node (Leaf 19) 1 (Leaf 9)))))
tree = Node left_child1 1 right_child1
     where
      left_child1  = Node2 2 (Leaf 2)
      right_child1 = Node2 5 (Leaf 9)

is_binary_search_tree :: Tree -> Bool
is_binary_search_tree (Node a b c) = 

main = print tree
