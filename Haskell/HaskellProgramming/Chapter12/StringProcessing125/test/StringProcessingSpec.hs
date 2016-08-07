module StringProcessingSpec (spec) where


import Lib
       ( notThe
       , replaceThe
       , replaceTheRecursive
       , countTheBeforeVowel
       , countVowels
       )

import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = do
    describe "notThe Test" $ do
        it "simple test" $ do
            notThe "the" `shouldBe` Just "the"

    describe "replaceThe Test" $ do
        it "simple test" $ do
            replaceThe "the cow loves us" `shouldBe` "a cow loves us"

        it "simple test, recursive" $ do
            replaceTheRecursive "the cow loves us" `shouldBe` "a cow loves us"

    describe "countTheBeforeVowel Test" $ do
        it "simple test" $ do
            countTheBeforeVowel "the cow" `shouldBe` 0

        it "simple test" $ do
            countTheBeforeVowel "the evil cow" `shouldBe` 1

    describe "countVowels Test" $ do
        it "simple test" $ do
            countVowels "the cow" `shouldBe` 2

        it "simple test" $ do
            countVowels "Mikolajczak" `shouldBe` 4
