module Lib
    ( IgnoreOne (..)
    ) where

{- The data constructor looks like we are applying a function
 - f to an argument a, but that is NOT correct. On the right
 - hand side of a data constructor, only TYPES show up, hence
 - (f a) as a whole is a type, NOT function apllication.
 - We can see from that, since only complete types are allowed
 - on the r.h.s., that f must have kindness * -> *...
 -}
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
    deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething a b) = IgnoringSomething a (fmap f b)
