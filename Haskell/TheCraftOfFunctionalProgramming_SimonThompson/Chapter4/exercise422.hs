module Exercise422 where

import Test.QuickCheck hiding (Result)


type Func = Integer -> Integer

ex22 :: Func -> Integer -> Bool
ex22 f n = 
    let ex22_helper f i = if i < n then
                              (||) (f i == 0) (ex22_helper f (i + 1))
                          else
                              f n == 0
    in
    ex22_helper f 1

test_func 0 = 1
test_func 1 = 44
test_func 2 = 17
test_func 3 = 45
test_func _ = 0

main :: IO ()
main = do
        print (ex22 (\x -> x `mod` 2) 2)
        print (ex22 test_func 1)
        print (ex22 test_func 2)
        print (ex22 test_func 3)
        print (ex22 test_func 4)
