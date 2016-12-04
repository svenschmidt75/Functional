module Lib
    ( List (..)
    ) where

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
-- (<$>) :: (a -> b) -> f    a -> f    b
-- (<$>) :: (a -> b) -> List a -> List b
    fmap _ Nil              = Nil
    fmap f (Cons head tail) = Cons (f head) (f <$> tail)

concat' :: List a -> List a -> List a
concat' Nil          ls  = ls
concat' ls           Nil = ls
concat' (Cons lh lt) l2  = Cons lh (concat' lt l2)

foldl'' :: (List b -> List a -> List b) -> List b -> List (List a) -> List b
foldl'' _ accum Nil          = accum
foldl'' f accum (Cons lh ls) = foldl'' f (f accum lh) ls

foldr'' :: (List a -> List b -> List b) -> List b -> List (List a) -> List b
foldr'' _ accum Nil          = accum
foldr'' f accum (Cons lh ls) = f lh (foldr'' f accum ls)

join' :: List (List a) -> List a
join' Nil = Nil
join' ls  = foldl'' concat' Nil ls
--join' ls  = foldr'' concat' Nil ls

instance Applicative List where
    pure a = Cons a Nil

-- (<*>) :: f    (a -> b) -> f    a -> f    b
-- (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil             _               = Nil
    (<*>) _               Nil             = Nil
    (<*>) (Cons f fsTail) xs = join' $ Cons (f <$> xs) (Cons (fsTail <*> xs) Nil)

instance Monad List where
    return = pure
-- (>>=) :: m     a -> (a -> m    b) -> m    b
-- (>>=) :: List  a -> (a -> List b) -> List b
    (>>=) Nil              _ = Nil
--    (>>=) (Cons head tail) f = join' $ Cons (f head) (Cons (tail >>= f) Nil)
    (>>=) ls f = join' $ f <$> ls
