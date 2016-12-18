module Lib
    ( Constant (..)
    ) where

data Constant a b = Constant a

instance Foldable (Constant a) where
    -- foldMap or foldr
    -- foldr :: (b -> c -> c) -> c -> t b -> c
    foldr _ accum (Constant _) = accum
