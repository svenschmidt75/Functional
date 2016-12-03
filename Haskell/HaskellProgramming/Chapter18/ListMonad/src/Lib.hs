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

foldr'' :: (List b -> List a -> List b) -> List b -> List (List a) -> List b
foldr'' _ accum Nil          = accum
foldr'' f accum (Cons lh ls) = foldr'' f (f accum lh) ls

join' :: List (List a) -> List a
join' Nil = Nil
join' ls  = foldr'' concat' Nil ls

instance Applicative List where
    pure a = Cons a Nil

-- (<*>) :: f    (a -> b) -> f    a -> f    b
-- (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil             _               = Nil
    (<*>) _               Nil             = Nil
    (<*>) (Cons f fsTail) xs = join' $ Cons (applyF f xs) (Cons (fsTail <*> xs) Nil)
                               where
                                 -- applyF :: (a -> b) -> List a -> List b
                                 applyF f Nil         = Nil
                                 applyF f (Cons x xs) = Cons (f x) (applyF f xs)

instance Monad List where
    return = pure
-- (>>=) :: m     a -> (a -> m    b) -> m    b
-- (>>=) :: List  a -> (a -> List b) -> List b
    (>>=) Nil              _ = Nil
    (>>=) (Cons head tail) f = join' $ Cons (f head) (Cons (tail >>= f) Nil)
