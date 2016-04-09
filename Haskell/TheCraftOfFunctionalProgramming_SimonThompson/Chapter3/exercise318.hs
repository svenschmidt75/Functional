module Exercise318 where

import Data.Char
import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


onThreeLines :: String -> String -> String -> String
onThreeLines s1 s2 s3 = s1 ++ "\n" ++ s2 ++ "\n" ++ s3

--prop_exercise316 :: Char -> Bool
--prop_exercise316 ch = toUpperCase ch == toUpper ch

--main = quickCheck prop_exercise316

main :: IO ()
main = do
        putStrLn $ onThreeLines "s1" "s2" "s3"
