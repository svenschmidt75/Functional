module Main where


-- class Functor t => Traversable t where
--     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

foldMap :: (Traversable f, Monoid m) => (a -> m) -> f a -> m

foldMap f = constant . traverse (Constant . f)




main :: IO ()
main = someFunc
