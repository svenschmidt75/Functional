module Main where

excercise71 :: Num a => [a] -> a
excercise71 []    = 0
excercise71 (x:_) = x + 1

excercise72 :: Num a => [a] -> a
excercise72 []      = 0
excercise72 (x:y:_) = x + y
excercise72 (x:[])  = x

excercise73_71 :: Num a => [a] -> a
excercise73_71 xs
    = if null xs then
        0
      else
        1 + head xs

--excercise73_72 :: Num a => [a] -> a

excercise74 :: String -> Char
excercise74 st = if (null st) then
                    '\0'
                 else
                    head st



main = do
    print $ excercise71 [0, 1]
    print $ excercise72 [1, 2]
    print $ excercise73_71 [1, 2]
    print $ excercise74 "763"
