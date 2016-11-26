module Lib
    ( Person (..)
    ) where

data Person a = Person String String
    deriving (Eq, Show)
