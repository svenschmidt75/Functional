module Lib
    ( binarySearch
    ) where

import Prelude hiding (min, max)
import Control.Monad.Trans.State.Lazy ( State
                                      , state)

binarySearch :: Ord a => [a] -> a -> State Int Bool
binarySearch [] _ = return False
binarySearch xs a = binarySearch' xs a 0 (length xs - 1)

binarySearch' :: Ord a => [a] -> a -> Int -> Int -> State Int Bool
binarySearch' xs a min max
    | min > max = state $ \s -> (False, s + 1)
    | otherwise = if value < a then
                    binarySearch' xs a (mid + 1) max
                  else if value > a then
                    binarySearch' xs a min (mid - 1)
                  else
                    state $ \s -> (value == a, s + 1)
                  where
                    mid = (min + max) `div` 2
                    value = xs !! mid