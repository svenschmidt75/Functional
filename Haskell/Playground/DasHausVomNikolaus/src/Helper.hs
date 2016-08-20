module Helper
         ( replace
         ) where

replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace itemToReplace item (x:xs)
    | x == itemToReplace = item : replace itemToReplace item xs
    | otherwise          =    x : replace itemToReplace item xs
