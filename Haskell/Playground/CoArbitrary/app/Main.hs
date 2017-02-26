module Main where

import Test.QuickCheck
import Lib

-- based on https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html


-- write Arbitrary instance for return type of function
-- generateImage :: (Int -> Int) -> Int -> Image, i.e.
-- Image
genImage :: Gen Image
genImage = do
    -- generate function
    f <- arbitrary
    -- restrict count
    count <- arbitrary `suchThat` (\n -> n > 0 && n < 10)
    return $ generateImage f count

main :: IO ()
main = do
    -- resize restricts the range of random values used
    result <- generate $ resize 5 genImage
    print $ content result
