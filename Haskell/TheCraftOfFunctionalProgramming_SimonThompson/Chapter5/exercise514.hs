module Exercise514 where

import Test.QuickCheck hiding (Result)


data Address = ByNumber Int
             | ByName String

printAddress :: Address -> String
printAddress (ByNumber number) = show number
printAddress (ByName name) = name


main :: IO ()
main = do
    print (printAddress (ByNumber 10))
    print (printAddress (ByName "White Rock"))
