module Exercise320 where

import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromIntegral (a + b + c) / 3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = let average = averageThree a b c in
                            let lst = map fromInteger [a, b, c] in 
                            toInteger $ length $ filter (>average) lst

prop_exercise320 :: Integer -> Integer -> Integer -> Bool
prop_exercise320 a b c = averageThree a b c == fromInteger (sum [a, b, c]) / 3

prop_exercise321 :: Integer -> Integer -> Integer -> Bool
prop_exercise321 a b c = howManyAboveAverage a b c == 
                                                      let lst = map fromInteger [a, b, c] in
                                                      (toInteger $ length $ filter (>averageThree a b c) lst)

--main = quickCheck prop_exercise316

main :: IO ()
main = do
        print (averageThree 1 6 9)
        putStrLn $ show $ howManyAboveAverage 1 6 9
        quickCheck prop_exercise320
        quickCheck prop_exercise321