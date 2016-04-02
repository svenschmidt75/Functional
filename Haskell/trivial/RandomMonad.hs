import System.Random

f :: Int -> StdGen -> (Int, StdGen)
f a seed = let (x, seed2) = random seed in
           (a + x, seed2)

g :: Int -> StdGen -> (Int, StdGen)
g a seed = let (x, seed2) = random seed in
           (a + x, seed2)

bind :: (Int -> StdGen -> (Int, StdGen)) -> (StdGen -> (Int, StdGen)) -> (StdGen -> (Int, StdGen))
bind func x seed = let (x', seed') = x seed in func x' seed'
