module Lib
    ( Possibly (..)
    ) where

data Possibly a =
      LolNope
    | Yeppers a
    deriving (Eq, Show)

-- functor for sum type
-- all others sofar like Two, Three, Four etc. were product types
instance Functor Possibly where
    fmap f (LolNope)   = LolNope
    fmap f (Yeppers a) = Yeppers $ f a
