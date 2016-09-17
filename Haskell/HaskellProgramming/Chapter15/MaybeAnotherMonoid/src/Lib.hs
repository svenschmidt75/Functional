module Lib
    ( firstMappend
    , FstId
    , FirstMappend
    , First' (..)
    ) where

newtype First' a = First' { getFirst' :: Maybe a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty  = undefined
    mappend = undefined

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool
