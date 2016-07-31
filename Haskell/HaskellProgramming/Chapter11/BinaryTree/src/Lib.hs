module Lib
       ( someFunc
       , BinaryTree (..)
       , insert'
       , mapTree
       , preorder
       , inorder
       , postorder
       ) where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf  = Node Leaf a Leaf
insert' a (Node bl value br)
    | a == value = Node bl a br
    | a < value  = Node (insert' a bl) value br
    | otherwise  = Node bl             value (insert' a br)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf               = Leaf
mapTree f (Node bl value br) = Node (mapTree f bl) (f value) (mapTree f br)

-- 1. take current node's value
-- 2. take left child's value
-- 3. take right child's value
-- think: /
--        --
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node bl value br) = value : preorder bl ++ preorder br

-- 1. take left child's value
-- 2. take current node's value
-- 3. take right child's value
-- think: /\
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node bl value br) = preorder bl ++ [value] ++ preorder br

-- 1. take left child's value
-- 2. take right child's value
-- 3. take current node's value
-- think:  \
--        --
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node bl value br) = preorder bl ++ preorder br ++ [value]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
