module Exercise39 where

import Test.QuickCheck


-- ALT + F3: Select all occurances


exercise39 :: Integer -> Integer -> Integer -> Bool
exercise39 m n p = if m /= n then
                    if n /= p then
                     m /= p
                    else
                     False
                   else
                    False

prop_exercise39 :: Integer -> Integer -> Integer -> Bool
prop_exercise39 m n p = (exercise39 m n p) == (m /= n && m /= p && n /= p)

main = quickCheck prop_exercise39
