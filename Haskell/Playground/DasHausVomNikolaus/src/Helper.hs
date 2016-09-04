module Helper
         ( replace
         , replace'
         ) where

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace itemToReplace item xs =
    replace' (==) itemToReplace item xs

replace' :: Eq a => (a -> a -> Bool) -> a -> a -> [a] -> [a]
replace' _ _ _ [] = []
replace' p itemToReplace item (x:xs)
    | p x itemToReplace = item : replace' p itemToReplace item xs
    | otherwise         =    x : replace' p itemToReplace item xs
