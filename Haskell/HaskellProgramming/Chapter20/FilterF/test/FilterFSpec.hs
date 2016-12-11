module FilterFSpec (spec) where

import Test.Hspec
import Data.Monoid

import Lib

spec :: Spec
spec = do
    describe "filterF" $ do
        it "List" $ do
            let xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10 :: Int]
            let result = filterF odd xs :: Sum Int
            result `shouldBe` Sum 25
        it "from Typeclassopedia" $ do
            let xs = words "The quick brown fox jumps over the lazy fence"
            -- Get a list of all the Strings in a container which include the
            -- letter a.
            filterF (elem 'o') xs `shouldBe` ["brown", "fox", "over"]
