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

iter :: (a -> IO ()) -> BinaryTree a -> IO String
iter f binTree = 
    processSteps steps
    where
        linearize Leaf cont = cont
        linearize (Node x l r) cont = Step(x, linearize l (linearize r cont))

        steps = linearize binTree Finished

        processSteps Finished = return "Done"
        processSteps (Step(x, getNext)) = do
                                            f x
                                            processSteps getNext

main :: IO ()
main = do
    let binTree = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
    iter (\x -> putStrLn $ show x) binTree
    putStrLn "Done"
