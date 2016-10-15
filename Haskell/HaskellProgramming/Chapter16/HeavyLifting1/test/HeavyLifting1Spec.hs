module HeavyLifting1Spec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

-- Add fmap, parentheses, and function composition to the expression
-- as needed for the expression to typecheck and produce the expected
-- result.

spec :: Spec
spec = do
    describe "Heavy Lifting" $ do
        it "1" $ do
            let result = fmap (+1) $ read "[1]" :: [Int]
            result `shouldBe` [2]
        it "2" $ do
            -- two, nested contexts to lift over,
            -- outer: Maybe
            -- inner: []
            let result = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
            result `shouldBe` Just ["Hi,lol","Hellolol"]
        it "3" $ do
            let result = ($) ((*2) . (\x -> x - 2)) 1
            result `shouldBe` (-2)
        it "4" $ do
            let d = ((return '1' ++) . show) . (\x -> [x, 1..3])
            (d 0) `shouldBe` "1[0,1,2,3]"
        prop "5" $ do
            prop5

prop5 :: Property
prop5 = monadicIO $ do
    let e = let ioi = readIO "1" :: IO Integer
                changed = (fmap read $ fmap ("123" ++) (fmap show ioi)) :: IO Integer
            in fmap (*3) changed
    result <- run $ e
    assert $ result == 3693
