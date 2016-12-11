module Lib
    ( Three (..)
    ) where

data Three a b c = Three a b c

instance Foldable (Three a b) where
    -- foldMap or foldr
    -- foldr :: (b -> c -> c) -> c -> t b -> c
    foldr f accum (Three _ _ b) = f b accum
