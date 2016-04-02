module Failure where

data Err a =
      OK a
    | Error
    deriving Show

divBy :: Integer -> Integer -> Err Integer
divBy 0 y = Error
divBy x y = OK (div y x)

main = do
        print ((divBy 2 `composeErr` divBy 5) 2310)
        print ((divBy 0 `composeErr` divBy 7) 2310)
        print ((divBy 5 `composeErr` divBy 11) 2310)

composeErr :: (b -> Err c) -> (a -> Err b) -> (a -> Err c)
composeErr f g x = case g x of
                        OK y  -> f y
                        Error -> Error

idErr :: a -> Err a
idErr x = OK x

bindErr :: Err a -> (a -> Err b) -> Err b
bindErr e f = (composeErr f id) e

instance Functor Err where
  fmap f (OK x) = OK (f x)
--  fmap f Error  = Error

instance Applicative Err where
  pure = OK
  OK f <*> OK x = OK (f x)
-- OK f <*> Error = Error

instance Monad Err where
    return x = idErr x
    (>>=)    = bindErr 