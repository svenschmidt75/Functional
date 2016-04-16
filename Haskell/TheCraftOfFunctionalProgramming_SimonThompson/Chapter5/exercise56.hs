module Exercise56 where

import Test.QuickCheck hiding (Result)


--type ShopItem = (String, Int)

data ShopItem = ShopItem String Int



main :: IO ()
main = do
    print "done"
