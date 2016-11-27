module Lib
    ( Person (..)
    ) where

data Person = Person String String
    deriving (Eq, Show)
