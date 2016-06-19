module Main where

{- Programming in F# 3.0, Chris Smith
 - Example 7.17: Using continuations to implement operating
 - on a list in a tail-recursive fashion to avoid stack
 - overflow.
 - Translated to Haskell.
-}

data BinaryTree a = 
      Node a (BinaryTree a) (BinaryTree a)
    | Leaf

data ContinuationStep a =
      Finished
    | Step (a, ContinuationStep a)

iter :: a -> BinaryTree a -> ()
iter f binTree = 
    processSteps steps
    where
        linearize :: BinaryTree a -> ContinuationStep a -> ContinuationStep a
        linearize Leaf cont = cont
        linearize (Node x l r) cont = Step(x, linearize l (linearize r cont))

        steps :: ContinuationStep a
        steps = linearize binTree Finished

        processSteps :: ContinuationStep a -> ()
        processSteps Finished = ()
        processSteps Step(x, getNext) = let r = f x in
                                        processSteps getNext

main :: IO ()
main = do
    let binTree = Node 1 (Leaf) (Leaf)
    iter (+1) binTree
    print "Done"
