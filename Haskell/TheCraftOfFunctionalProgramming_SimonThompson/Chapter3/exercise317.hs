module Exercise317 where

import Data.Char
import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


toDigit :: Char -> Int
toDigit ch = case isDigit ch of
                val | val == True -> fromEnum ch - fromEnum '0'
                    | val == False -> 0
             where
                isDigit ch = fromEnum ch >= fromEnum '0' && fromEnum ch <= fromEnum '9'

--prop_exercise316 :: Char -> Bool
--prop_exercise316 ch = toUpperCase ch == toUpper ch

--main = quickCheck prop_exercise316

main :: IO Int
main = do
        return $ toDigit '6'
