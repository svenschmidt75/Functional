module Lib
    ( List (..)
    , ZipList' (..)
    ) where



data List a = Nil
            | Cons a (List a)
    deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
    fmap = undefined

instance Applicative List where
    pure = undefined
    (<*>) = undefined





newtype ZipList' a = ZipList' (List a)
    deriving (Eq, Show)

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure = undefined
    (<*>) = undefined
