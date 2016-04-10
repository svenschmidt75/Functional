module Exercise322 where

import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | b * b > disc = 2
    | b * b == disc = 1
    | b * b < disc = 0
    where
        disc = 4.0 * a * c

numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c = if a /= 0
                    then
                        numberNDroots a b c
                    else
                        if b /= 0
                        then
                            1
                        else
                            if c /= 0
                            then
                                0
                            else
                                3

roots :: Float -> Float -> Float -> (Float, Float)
roots a b c = if discriminant >= 0
              then
                let root1 = (-b - sqrt discriminant)/2.0/a in
                let root2 = (-b + sqrt discriminant)/2.0/a in
                (root1, root2)
              else
                (0, 0)
              where
                discriminant = 4.0 * a * c

--smallerRoot :: Float -> Float -> Float -> Integer
--largerRoot :: Float -> Float -> Float -> Integer

main :: IO ()
main = do
        print (roots 0 0 9)
