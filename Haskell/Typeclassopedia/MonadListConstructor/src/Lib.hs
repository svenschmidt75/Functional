module Lib
    ( MyList (..)
    ) where

newtype MyList a = MyList { unMyList :: [a] }
    deriving (Show, Eq)

instance Functor MyList where
    fmap _ (MyList []) = MyList []
    fmap f (MyList xs) = MyList $ map f xs

instance Applicative MyList where
    pure x                        = MyList [x]
    (<*>) (MyList fs) (MyList xs) = MyList $ [f x | f <- fs, x <- xs]

instance Monad MyList where
    return = undefined
    (>>=) = undefined
