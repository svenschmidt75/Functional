module State where

--import Control.Applicative (liftA3)
--import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random


f1 :: State StdGen Int
f1 = state $ \s -> (1, s)

f2 :: State StdGen Int
f2 = do
    -- get state
    s <- get

    -- insert an updated state
    put $ mkStdGen 1

    -- return value
    return 1

main :: IO ()
main = do
    let (r1, s1) = runState f1 (mkStdGen 0)
    print r1
