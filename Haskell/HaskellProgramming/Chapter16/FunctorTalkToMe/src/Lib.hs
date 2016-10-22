module Lib
    ( TalkToMe (..)
    ) where

import Text.Show.Functions

data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)
    deriving (Show)

instance Functor TalkToMe where
    fmap _ Halt        = Halt
    fmap f (Print a b) = Print a (f b)
    fmap f (Read g)    = Read $ f . g

instance Eq a => Eq (TalkToMe a) where
    Halt          == Halt          = True
    (Print a1 b1) == (Print a2 b2) = a1 == a2 && b1 == b2
-- this stinks, cannot compare functions like this
    (Read f)      == (Read g)      = True
