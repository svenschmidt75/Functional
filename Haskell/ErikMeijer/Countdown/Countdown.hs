module Countdown where

rotateInternal :: [Int] -> Int -> [[Int]]
rotateInternal [] _ = []
rotateInternal q@(x:xs) n
    | n > 0     = q : rotateInternal (xs ++ [x]) (n - 1)
    | otherwise = []

-- in: [1, 2, 3]
-- out: [[1, 2, 3], [2, 3, 1], [3, 1, 2]]
rotate :: [Int] -> [[Int]]
rotate xs = rotateInternal xs (length xs)

-- in: [[2, 3], [3, 2]] 1
-- out: [[1, 2, 3], [1, 3, 2]]
prefix :: [[Int]] -> Int -> [[Int]]
prefix xs x = map (x:) xs

permutations :: [Int] -> [[Int]]
permutations [] = [[]]
permutations  q = let rotated = rotate q in
                  concatMap (\(x:xs) -> prefix (permutations xs) x) rotated

type Step = (String, Int)
type Steps = [Step]

addSteps :: Int -> Int -> Steps -> Steps
addSteps x y steps = if x + y > 0 then ((show x ++ "+" ++ show y), x + y) : steps else steps

subSteps :: Int -> Int -> Steps -> Steps
subSteps x y steps = if x - y > 0 then ((show x ++ "-" ++ show y), x - y) : steps else steps

countdown :: [Int] -> [Steps] -> [Steps]
countdown (_:[]) steps = steps
countdown (x:y:xs) (as:ss:ms:ds) = let a = countdown ((x+y):xs) (adds:ss:ms:ds) in
                                   let b = countdown ((x-y):xs) (as:subs:ms:ds) in
                                   b
                    where
                        adds = addSteps x y as
                        subs = subSteps x y ss

