module Lib
    ( sequenceAL
    ) where

{- Example:
 - [Just 1, Just 6, Just 10] -> Just [1, 6, 10]
 - [Just 1, Nothing, Just 10] -> Nothing
 -}
sequenceAL :: Applicative f => [f a] -> f [a]
sequenceAL [] = pure []
sequenceAL (x:xs) = let f1 = lift x
                        f2 = sequenceAL xs
                    in
                        -- Just [1] -> Just [2] -> Just ([1] (++) [2]) = Just [1, 2]
                        pure (++) <*> f1 <*> f2
                    where
                        lift :: Applicative f => f a -> f [a]
                        -- Just 1 -> Just [1]
                        lift x = fmap (:[]) x
