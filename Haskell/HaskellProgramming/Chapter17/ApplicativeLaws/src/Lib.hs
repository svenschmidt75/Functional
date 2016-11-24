module Lib
    ( Bull (..)
    ) where

data Bull = Fools
          | Twoo
    deriving (Eq, Show)

instance Monoid Bull where
    mempty            = Twoo
    mappend Twoo Twoo = Twoo
    mappend _    _    = Fools
