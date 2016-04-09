module Exercise316 where

import Data.Char
import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master


toUpperCase :: Char -> Char
toUpperCase ch = if isLowerCase ch then
                    toEnum $ fromEnum 'A' + (fromEnum ch - fromEnum 'a')
                 else
                    ch
                 where
                 isLowerCase ch = fromEnum ch >= fromEnum 'a' && fromEnum ch <= fromEnum 'z'

--prop_exercise316 :: Char -> Bool
--prop_exercise316 ch = toUpperCase ch == toUpper ch

--main = quickCheck prop_exercise316

main :: IO Char
main = do
        return $ toUpperCase 'd'
