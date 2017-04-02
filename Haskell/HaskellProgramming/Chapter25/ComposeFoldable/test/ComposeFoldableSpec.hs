module ComposeFoldableSpec (spec) where

import Test.Hspec

import Lib
    ( Compose (..)
    )

spec :: Spec
spec = do
    describe "Compose laws" $ do
        it "Functor" $ do
            let value = Compose $ Just [1] :: Compose Maybe [] Int
            let result = (+1) <$> value
            result `shouldBe` (Compose $ Just [2])

        it "Applicative" $ do
            let value = Compose $ Just [1] :: Compose Maybe [] Int
            let f = Compose $ Just [(+1), (+(-1))]
            let result = f <*> value
            result `shouldBe` (Compose $ Just [2, 0])
