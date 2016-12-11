module Lib
    ( Two (..)
    ) where

data Two a b = Two a b

instance Foldable (Two a) where
    -- foldMap or foldr
    -- foldr :: (b -> c -> c) -> c -> t b -> c
    foldr f accum (Two _ b) = f b accum
