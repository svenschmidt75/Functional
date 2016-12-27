module LiftA2Spec (spec) where

import Test.Hspec

import Lib


spec :: Spec
spec = do
    describe "(->) r Applicative" $ do
        it "getDog" $
            getDog pers `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
        it "getDogR" $
            getDogR pers `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
        it "getDogR'" $
            getDogR' pers `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
        it "getDogR''" $
            getDogR'' pers `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
