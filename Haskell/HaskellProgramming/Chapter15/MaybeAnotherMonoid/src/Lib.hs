module Lib
    ( FstId
    , FirstMappend
    , First' (..)
    ) where

newtype First' a = First' { getFirst' :: Maybe a }
    deriving (Eq, Show)

instance Monoid (First' a) where
    mempty  = First' Nothing
    arg1@(First' (Just _)) `mappend` _ = arg1
    (First' Nothing)       `mappend` a = a

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool
