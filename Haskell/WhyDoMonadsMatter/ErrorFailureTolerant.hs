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

composeErr :: (Integer -> Err Integer) -> (Integer -> Err Integer) -> (Integer -> Err Integer)
composeErr f g x = case g x of
                        OK y  -> f y
                        Error -> Error
