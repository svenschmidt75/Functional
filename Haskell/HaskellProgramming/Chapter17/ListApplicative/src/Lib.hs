module Lib
    ( List (..)
    , applyOne
    , concat'
    , prepend
    , flatten
    ) where

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

applyOne :: (a -> b) -> List a -> List b
applyOne f Nil         = Nil
applyOne f (Cons a as) = Cons (f a) (applyOne f as)

-- prepend an item to a list
prepend :: a -> List a -> List a
prepend _ Nil = Nil
prepend a as  = Cons a as

concat' :: List a -> List a -> List a
concat' Nil xs         = xs
concat' xs   Nil       = xs
concat' (Cons a as) bs = concat' as (prepend a bs)

flatten :: [List a] -> List a
flatten [] = Nil
flatten (x:y:xs) = concat' (concat' x y) (flatten xs)
flatten (x:xs) = concat' x (flatten xs)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _   = Nil
    (<*>) _   Nil = Nil
    (<*>) (Cons f Nil) xs = applyOne f xs
    (<*>) (Cons f fs) xs = concat' (fs <*> xs) (applyOne f xs)
