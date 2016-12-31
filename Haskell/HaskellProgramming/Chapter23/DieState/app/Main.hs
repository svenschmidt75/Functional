module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random


-- Six-sided die
data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = case n of
                1 -> DieOne
                2 -> DieTwo
                3 -> DieThree
                4 -> DieFour
                5 -> DieFive
                6 -> DieSix
                -- Use this tactic _extremely_ sparingly.
                x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    -- this will produce the same results every
    -- time because it is free of effects.
    -- This is fine for this demonstration.
    let s        = mkStdGen 0
        (d1, s1) = randomR (1, 6) s
        (d2, s2) = randomR (1, 6) s1
        (d3, _)  = randomR (1, 6) s2
    (intToDie d1, intToDie d2, intToDie d3)


rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)
-- rollDie = state $ randomR (1, 6)
--               >>= \(n, s) -> return (intToDie n, s)
-- state $ randomR (1, 6) => State StdGen (Int, Int)
-- State $ \s -> randomR (1, 6) s
-- The type of 'randomR (1, 6) >>= \(n, s) -> return (intToDie n, s)'
-- is 'RandomGen t => t -> (Die, t)'
-- The type of 'randomR (1, 6)' is '(Random a, RandomGen g, Num a) => g -> (a, g)'

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= 20 = count
            | otherwise = let (die, nextGen) = randomR (1, 6) gen
                          in go (sum + die) (count + 1) nextGen

main :: IO ()
main = do
-- (DieSix,DieSix,DieFour)
    print $ evalState rollDieThreeTimes' (mkStdGen 0)
-- (DieSix,DieFive,DieTwo)
    print $ evalState rollDieThreeTimes' (mkStdGen 1)
-- [DieSix,DieSix,DieSix,DieSix,DieSix,DieSix]
    print $ take 6 $ evalState infiniteDie (mkStdGen 0)
-- [DieSix,DieSix,DieFour,DieOne,DieFive]
    print $ evalState (nDie 5) (mkStdGen 0)
-- [DieSix,DieFive,DieTwo,DieSix,DieFive]
    print $ evalState (nDie 5) (mkStdGen 1)
-- 5
    print $ rollsToGetTwenty (mkStdGen 0)
-- 5
    print $ rollsToGetTwenty (mkStdGen 0)
    ((rollsToGetTwenty . mkStdGen) <$> randomIO) >>= print
    (rollsToGetTwenty . mkStdGen) <$> randomIO >>= print
