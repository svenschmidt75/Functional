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

instance Applicative List where
    pure a = Cons a Nil

-- (<*>) :: f    (a -> b) -> f    a -> f    b
-- (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil             _               = Nil
    (<*>) _               Nil             = Nil
    (<*>) (Cons f fsTail) (Cons x xsTail) = Cons (f x) (fsTail <*> xsTail)

instance Monad List where
    return = pure
-- (>>=) :: m     a -> (a -> m    b) -> m    b
-- (>>=) :: List  a -> (a -> List b) -> List b
    (>>=) Nil _ = Nil
    (>>=) (Cons head tail) f = concat' $ Cons $ (f head) (tail >>= f)

concat' :: List (List a) -> List a
concat' = undefined
