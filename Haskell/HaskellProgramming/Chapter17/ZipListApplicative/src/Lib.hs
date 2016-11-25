module Lib
    ( List (..)
    , ZipList' (..)
    , repeat'
    ) where



data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) $ f <$> as

-- append the 2nd list to the 1st
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

-- fold: operation to execute between two Cons elements
-- in this case, we take two lists and merge them into a new list,
-- the new merged list is kept in the accumulator
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
-- flatMap monadic bind?
-- bind :: m a -> (a -> m b) -> m b
-- yes, m = List here...
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ f <$> as

instance Applicative List where
    pure a    = Cons a Nil
    fs <*> xs = flatMap (\f -> fmap f xs) fs

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)


newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipWith' :: List (a -> b) -> List a -> List b
zipWith' (Cons f fs) (Cons x xs) = Cons (f x) $ zipWith' fs xs
zipWith' Nil         _           = Nil
zipWith' _           Nil         = Nil

instance Applicative ZipList' where
    pure a                      = ZipList' $ Cons a Nil
    ZipList' fs <*> ZipList' xs = ZipList' $ zipWith' fs xs
