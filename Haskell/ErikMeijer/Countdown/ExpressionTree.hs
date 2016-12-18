module Main where

data Tree = Node Op Tree Tree
          | Leaf Int
          deriving (Show)

data Op = Add
        | Sub
        | Mul
        | Div
        deriving (Show)

{- Input:
 - 1. List of integers to create an expression tree for
 - 2. List of expression trees generated, initially empty
 - Output:
 - List of all generated expression trees
 -
 - Note: Some of the generated expression trees are equivalent in the sense
 -       that the resulting algebraic expression is the same.
 -       More so, since we later use all permutations of these numbers,
 -       we will create tons of redundant expression trees. This is to be
 -       cleaned up at a later stage.
 -       Example:
 -       The expression tree for +1+3*(-4) is equivalent to the one generated
 -       from +1-3*4. Currently, we generate both.
-}
generateExpressionTree :: [Int] -> [Tree]
generateExpressionTree       [] = error "Should never happen"
generateExpressionTree       xs = generateExpressionTree_internal xs []

generateExpressionTree_internal :: [Int] -> [Tree] -> [Tree]
generateExpressionTree_internal       []        _ = error "Should never happen"
generateExpressionTree_internal      [x] subtrees = let sta = Leaf x    : subtrees in
                                                    let sts = Leaf (-x) : subtrees in
                                                    sta ++ sts
generateExpressionTree_internal (x:y:xs) trees =
                      let sta1 = map (\z -> Node Add (Leaf x   ) z) subtrees in
                      let sta2 = map (\z -> Node Add (Leaf (-x)) z) subtrees in
                      let stb1 = map (\z -> Node Sub (Leaf x   ) z) subtrees in
                      let stb2 = map (\z -> Node Sub (Leaf (-x)) z) subtrees in
                      let stc1 = map (\z -> Node Mul (Leaf x   ) z) subtrees in
                      let stc2 = map (\z -> Node Mul (Leaf (-x)) z) subtrees in
                      let std1 = map (\z -> Node Div (Leaf x   ) z) subtrees in
                      let std2 = map (\z -> Node Div (Leaf (-x)) z) subtrees in
                      sta1 ++ sta2 ++ stb1 ++ stb2 ++ stc1 ++ stc2 ++ std1 ++ std2
                      where
                        subtrees = generateExpressionTree_internal (y:xs) trees

evaluateExpressionTree :: Tree -> Int
evaluateExpressionTree (Leaf x) = x
evaluateExpressionTree (Node Add l r) = evaluateExpressionTree l + evaluateExpressionTree r
evaluateExpressionTree (Node Sub l r) = evaluateExpressionTree l - evaluateExpressionTree r
evaluateExpressionTree (Node Mul l r) = evaluateExpressionTree l * evaluateExpressionTree r
evaluateExpressionTree subtree@(Node Div l r)
    | evaluateExpressionTree r == 0 = error (show ("Failing subtree: " ++ (describeExpressionTree subtree)))
    | otherwise                     = evaluateExpressionTree l `div` evaluateExpressionTree r

-- Print the expression tree
describeExpressionTree :: Tree -> String
describeExpressionTree (Leaf x)
    | x < 0     = "(" ++ show x ++ ")"
    | otherwise = show x
describeExpressionTree (Node Add l r) = "(" ++ describeExpressionTree l ++ "+" ++ describeExpressionTree r ++ ")"
describeExpressionTree (Node Sub l r) = "(" ++ describeExpressionTree l ++ "-" ++ describeExpressionTree r ++ ")"
describeExpressionTree (Node Mul l r) = "(" ++ describeExpressionTree l ++ "*" ++ describeExpressionTree r ++ ")"
describeExpressionTree (Node Div l r) = "(" ++ describeExpressionTree l ++ "/" ++ describeExpressionTree r ++ ")"

-- Check whether an expression tree has a node that divides by 0
hasInvalidDivision :: Tree -> Bool
hasInvalidDivision (Leaf _)                     = False
hasInvalidDivision (Node Div l r)               = if hasInvalidDivision l || hasInvalidDivision r then
                                                    True
                                                  else
                                                    m * y /= x
                                                    where
                                                      x = evaluateExpressionTree l
                                                      y = evaluateExpressionTree r
                                                      m = quot x y
hasInvalidDivision (Node _ l r)                 = hasInvalidDivision l || hasInvalidDivision r

-- Check whether an expression tree has a subexpression that evaluates to
-- <= 0
hasNonPositiveSubExtression :: Tree -> Bool
hasNonPositiveSubExtression (Leaf x) = x <= 0
hasNonPositiveSubExtression subtree@(Node Sub l r) = hasNonPositiveSubExtression l ||
                                                     hasNonPositiveSubExtression r ||
                                                     isValidSubTree == False
                                                     where
                                                        isValidSubTree = hasInvalidDivision l == False &&
                                                                         hasInvalidDivision r == False && 
                                                                         evaluateExpressionTree subtree > 0
hasNonPositiveSubExtression (Node _ l r) = hasNonPositiveSubExtression l || hasNonPositiveSubExtression r


-- Rotates a list of ints.
-- Examples:
--      [1, 2, 3] 1 -> [[1, 2, 3]]
--      [1, 2, 3] 2 -> [[1, 2, 3], [2, 3, 1]]
--      [1, 2, 3] 3 -> [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
rotateInternal :: [Int] -> Int -> [[Int]]
rotateInternal [] _ = []
rotateInternal q@(x:xs) n
    | n > 0     = q : rotateInternal (xs ++ [x]) (n - 1)
    | otherwise = []

-- Rotates a list of ints.
-- Examples:
--      [1, 2, 3] -> [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
rotate :: [Int] -> [[Int]]
rotate xs = rotateInternal xs (length xs)

-- Prefix a list of list of numbers with a number.
-- Examples:
--      [[2, 3], [3, 2]] 1 -> [[1, 2, 3], [1, 3, 2]]
prefix :: [[Int]] -> Int -> [[Int]]
prefix xs x = map (x:) xs

-- Generate all permutations for a list of numbers
permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations  q = let rotated = rotate q in
                  concatMap (\(x:xs) -> prefix (permutations xs) x) rotated

main :: IO ()
main = do
        let numbers = [1, 3, 7, 10, 25, 50]
        let perm = permutations numbers
        let trees = concat $ map generateExpressionTree perm
        let trees2 = filter (not . hasNonPositiveSubExtression) trees
        let trees3 = filter (not . hasInvalidDivision) trees2
        let trees4 = map (\x -> (describeExpressionTree x, evaluateExpressionTree x)) trees3
        let trees5 = filter (\(_, y) -> y == 765) trees4
        print trees5
