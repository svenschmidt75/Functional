module Lib
    ( someFunc
    ) where

data FInListInTupleInMaybe a = FInListInTupleInMaybe [(Maybe a, String)]
                               deriving (Show)

instance Functor (FInListInTupleInMaybe) where
--    fmap :: Functor f => (a -> c) -> [(Maybe a, b)] -> [(Maybe c, b)]
    fmap f (FInListInTupleInMaybe input) = FInListInTupleInMaybe (doit input)
                                                where 
                                                    doit []         = []
                                                    doit ((x,s):xs) = (fmap f x, s) : doit xs

someFunc :: IO ()
someFunc = do
         let d = FInListInTupleInMaybe [(Just 1, "Nothing")]
         print $ fmap (\x -> x + 1) d
