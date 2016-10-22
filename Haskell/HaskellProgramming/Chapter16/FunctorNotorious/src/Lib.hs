module Lib
    ( Notorious (..)
    ) where


-- :k g :: * -> *
data Notorious g o a t = Notorious (g o) (g a) (g t)
    deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap f (Notorious a b c) = Notorious a b (fmap f c)
