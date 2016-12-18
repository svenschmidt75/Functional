module Lib
    ( List (..)
    ) where

data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
-- fmap :: (a -> b) -> f a -> f b
-- or
-- fmap :: (a -> f b) -> t a -> t (f b) (almost the same signature as traverse!)
    fmap _ Nil         = Nil
    fmap f (Cons a as) = Cons (f a) (f <$> as)

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

instance Foldable List where
-- foldr :: (a -> b -> b) -> b -> t a -> b
    foldr _ accum Nil         = accum
    foldr f accum (Cons a as) = f a (foldr f accum as)

instance Traversable List where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse g Nil         = pure Nil
    traverse g (Cons a as) = Cons <$> g a <*> traverse g as

{-
g a :: f b

Cons <$> g a = f (Cons b) :: f (List b -> List b)

traverse g as :: f (List b)

so

f (Cons b) <*> f (List b) :: f (List b)

-}
