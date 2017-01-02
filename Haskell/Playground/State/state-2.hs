module State where

--import Control.Applicative (liftA3)
--import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random


f1 :: State StdGen Int
f1 = state $ \s -> (1, s)

-- Read 'State s a' as a function s -> (a, s). State is nothing more than
-- a wrapper around functions of type s -> (a, s). By wrapping a function
-- of signature s -> (a, s), we can implement typeclasses like functor,
-- applicative and monad, adding certain capabilities like threading state
-- through subsequent function applications.
-- Reader, on the other hand, wraps functions of type r -> a, and the
-- implementation of functor, applicative and monad for it gives it different
-- semmantics compared to State.
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
