module Lib
    ( someFunc,
      BinaryTree (..),
      insert',
      mapTree,
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
