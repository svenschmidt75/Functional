module Main where

import Lib

main :: IO ()
main = do
--    e :: IO Integer
    let e = let ioi = readIO "1" :: IO Integer
                changed = (fmap read $ fmap ("123" ++) (fmap show ioi)) :: IO Integer
            in fmap (*3) changed
    result <- e
    -- show be "3693"
    print result
