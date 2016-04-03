module Exercise34 where

--import Prelude hiding ((&&))
import Test.QuickCheck


-- ALT + F3: Select all occurances


exercise34 :: Bool -> Bool -> Bool
exercise34 True True   = True
exercise34 _ _         = False

prop_exercise34 :: Bool -> Bool -> Bool
prop_exercise34 x y = (exercise34 x y) == (x && y)

main = quickCheck prop_exercise34
