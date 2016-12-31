module State where

--import Control.Applicative (liftA3)
--import Control.Monad (replicateM)
--import Control.Monad.Trans.State
import System.Random


newtype MyState s a = MyState { runState :: s -> (a, s) }

-- instance Functor (MyState s) where
-- -- fmap :: (a -> b) -> f         a -> f         b
-- -- fmap :: (a -> b) -> MyState s a -> MyState s b
--    fmap f (MyState sas) = MyState $ \s -> let (a, s') = sas s
--                                       in (f a, s')

-- instance Applicative (MyState s) where
-- -- pure :: a -> f       a
-- -- pure :: a -> MyState s a
--     pure a = MyState $ \s -> (a, s)

-- -- (<*>) :: f         (a -> b) -> f         a -> f         b
-- -- (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
--     (<*>) (MyState sab) (MyState sa) = MyState $ \s -> let (a, s')  = sa s
--                                                            (f, s'') = sab s'
--                                                        in (f a, s'')

-- instance Monad (MyState s) where
--     return = pure

-- -- (>>=) :: m         a -> (a -> m         b) -> m         b
-- -- (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
--     (>>=) (MyState sa) f = MyState $ \s -> let (a, s') = sa s
--                                                applied = runState $ f a
--                                            in applied s'

mkState :: (s -> (a, s)) -> MyState s a
mkState f = MyState $ \s -> f s


data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use this tactic _extremely_ sparingly.
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDie :: MyState StdGen Die
rollDie = mkState $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: MyState StdGen Die
rollDie' = mkState $ randomR (1, 6)
                >>= \(n, s) -> return (intToDie n, s)

-- using the Reader monad, (->) r
rollDie'' :: MyState StdGen Die
rollDie'' = MyState $ \r -> let (n, s) = randomR (1, 6) r
                            in (intToDie n, s)

-- randomR (1, 6) >>= \(n, s) -> return (intToDie n, s)
--                               ^^^^^^^^^^^^^^^^^^^^^^
-- The expression 'return (intToDie n, s)' is a function
-- that takes an argument, which it ignores, and returns
-- tuple (intToDie n, s). Signature: \_ -> (intToDie n, s)
-- The expression 'randomR (1, 6)' has signature g -> (a, g).
-- The whole expression uses the bind method of the reader
-- monad,
-- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b), see below.
-- Basically, the expression returns a function (return value
-- of bind expression, r -> b), that takes an r, passes that r
-- to 'randomR (1, 6)', and shoves the result of that, tuple
-- (n, s) to the function, that basically ignores the 1st r,
-- and returns the new state s, together with intToDie n.
-- NOTE: The do expression does NOT use the State monad!!!, but
-- instead the reader monad.

-- Monad for (->) r:
-- return a :: a -> m a
-- return a :: a -> (->) r a
-- return a :: a -> (r -> a)
-- (>>=) :: m      a -> (a -> m      b) -> m      b
-- (>>=) :: (->) r a -> (a -> (->) r b) -> (->) r b
-- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)

main :: IO ()
main = do
    print $ runState rollDie (mkStdGen 0)
    print $ runState rollDie' (mkStdGen 0)
    print $ runState rollDie'' (mkStdGen 0)
