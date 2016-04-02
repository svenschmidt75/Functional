module Dependence where

-- external configuration parameter we'd like to get rid of
i :: Integer
i = 3

main = putStrLn (f "foo")
        where f = left . right

-- left depends on external parameter i
left :: String -> String
left s = (repeatString i "< ") ++ s

-- right depends on external parameter i
right :: String -> String
right s = s ++ (repeatString i " >")

repeatString :: Integer -> String -> String
repeatString count str = if count <= 0
                            then ""
                            else str ++ (repeatString (count - 1) str)
