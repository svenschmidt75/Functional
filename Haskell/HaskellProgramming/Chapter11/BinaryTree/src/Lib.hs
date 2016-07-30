module Lib
    ( someFunc,
      BinaryTree (..),
      insert',
    ) where

data BinaryTree a = 
	Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: a -> BinaryTree a -> BinaryTree a
insert' a (Leaf)  = Node Leaf a Leaf




someFunc :: IO ()
someFunc = putStrLn "someFunc"
