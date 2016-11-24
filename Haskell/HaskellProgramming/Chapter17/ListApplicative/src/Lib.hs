module Lib
    ( List (..)
    ) where

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

applyOne :: (a -> b) -> List a -> List b
applyOne _ Nil         = Nil
applyOne f (Cons a as) = Cons (f a) (applyOne f as)

-- prepend an item to a list
prepend :: a -> List a -> List a
prepend _ Nil = Nil
prepend a as  = Cons a as

concat' :: List a -> List a -> List a
concat' Nil xs         = xs
concat' xs  Nil        = xs
concat' (Cons a as) bs = prepend a $ concat' as bs

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil         _   = Nil
    (<*>) _           Nil = Nil
    (<*>) (Cons f fs) xs  = concat' (applyOne f xs) (fs <*> xs)
