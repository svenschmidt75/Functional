module Exercise322 where

import Test.QuickCheck


-- ALT + F3: Select all occurances
-- git status
-- git add .
-- git commit -m "First commit"
-- git push origin master

numberNDroots :: Float -> Float -> Float -> Integer
numberNDroots a b c
    | b * b > disc  = 2
    | b * b == disc = 1
    | b * b < disc  = 0
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
roots a b c = if discriminant >= 0 then
                if a == 0 then
                    if b == 0 then
                        (c, 0)
                    else
                        (-c/b, 0)
                else
                    let root1 = (-b - sqrt discriminant)/2.0/a in
                    let root2 = (-b + sqrt discriminant)/2.0/a in
                    (root1, root2)
              else
                (0, 0)
              where
                discriminant = b * b - 4.0 * a * c

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = fst $ roots a b c

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = snd $ roots a b c

prop_exercise325 :: Float -> Float -> Float -> Bool
prop_exercise325 a b c = if numberRoots a b c /= 2 then
                            True
                         else
                            let (s, l) = roots a b c in
                            let p1 = a * s * s + b * s + c in
                            let p2 = a * l * l + b * l + c in
                            p1 == 0 && p2 == 0

main :: IO ()
main = do
        print (roots 5 1 (-1))
        quickCheck prop_exercise325

{-
a = 1E-45
b = 0
c = -0.12500004
(-1.3356869e22,1.3356869e22)
-}