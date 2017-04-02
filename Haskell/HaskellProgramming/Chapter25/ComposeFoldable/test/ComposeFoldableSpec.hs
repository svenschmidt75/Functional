module ComposeFoldableSpec (spec) where

import Test.Hspec
import Data.Monoid (Sum (..))

import Lib
    ( Compose (..)
    )


spec :: Spec
spec =
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

        it "Foldable" $ do
            let value = Compose $ Just [Sum 1, Sum 2, Sum 3] :: Compose Maybe [] (Sum Int)
            let result = foldMap id value
            result `shouldBe` (Sum 6)

        it "Traversable" $ do
            let value = Compose $ Just [1, 2, 3] :: Compose Maybe [] Int
            let result = traverse (Just . show) value
            result `shouldBe` (Just $ Compose (Just ["1", "2", "3"]))
