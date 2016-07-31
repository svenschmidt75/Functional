module Lib
    ( someFunc,
      BinaryTree (..),
      insert',
    ) where

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' a Leaf  = Node Leaf a Leaf
insert' a (Node bl value br)
    | a <= value = Node (insert' a bl) value br
    | otherwise  = Node bl             value (insert' a br)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
