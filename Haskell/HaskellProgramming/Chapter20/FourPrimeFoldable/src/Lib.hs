module Lib
    ( Four' (..)
    ) where

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    -- foldMap or foldr
    -- foldr :: (b -> c -> c) -> c -> t b -> c
    foldr f accum (Four' _ _ _ b3) = f b3 accum
