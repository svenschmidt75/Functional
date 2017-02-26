module Lib
    ( Image (..)
    , generateImage
    )
    where

newtype Image = Image { content :: [Int] }

generateImage :: (Int -> Int) -> Int -> Image
generateImage f count = Image $ generateImage' count
    where
        generateImage' n
            | n > 0     = f n : generateImage' (n - 1)
            | otherwise = []
