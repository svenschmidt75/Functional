module Lib
    ( someFunc
    ) where


-- wrap 'Maybe a' so we can define another instance of Monoid
newtype First a = First { getFirst :: Maybe a }
                  deriving (Show)

instance Monoid (First a) where
    mempty                                                                    = First { getFirst = Nothing }
    mappend f@(First { getFirst = Just x })    (First { getFirst = Nothing }) = f
    mappend   (First { getFirst = Nothing }) f@(First { getFirst = Just x })  = f
    mappend   (First { getFirst = Nothing })   (First { getFirst = Nothing }) = mempty
    mappend f@(First { getFirst = m1 })        (First { getFirst = m2 })      = f


someFunc :: IO ()
someFunc = do
    let f1 = First { getFirst = Just 1 }
    let f2 = First { getFirst = Nothing }
    let f3 = First { getFirst = Nothing }
    print $ f1 `mappend` f2
    print $ f2 `mappend` f1
    print $ f2 `mappend` (f1 `mappend` f3)
    putStrLn "someFunc"
