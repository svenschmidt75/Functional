module Lib
    ( Three' (..)
    ) where

data Three' a b = Three' a b b

instance Foldable (Three' a) where
    -- foldMap or foldr
    -- foldr :: (b -> c -> c) -> c -> t b -> c
    foldr f accum (Three' _ _ b2) = f b2 accum
