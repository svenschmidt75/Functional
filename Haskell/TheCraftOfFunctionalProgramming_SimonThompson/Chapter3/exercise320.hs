module Exercise320 where

import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = fromIntegral (a + b + c) / 3.0

--prop_exercise316 :: Char -> Bool
--prop_exercise316 ch = toUpperCase ch == toUpper ch

--main = quickCheck prop_exercise316

main :: IO ()
main = do
        putStrLn $ show $ averageThree 1 2 9
