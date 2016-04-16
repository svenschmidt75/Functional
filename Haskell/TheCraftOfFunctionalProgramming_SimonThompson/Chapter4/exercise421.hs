module Exercise421 where

import Test.QuickCheck hiding (Result)


type Func = Integer -> Integer

ex21 :: Func -> Integer -> Integer
ex21 f n = 
    let ex21_helper f i = if i < n then
                              max (f i) (ex21_helper f (i + 1))
                          else
                              f n
    in
    ex21_helper f 0

test_func 0 = 0
test_func 1 = 44
test_func 2 = 17
test_func 3 = 45
test_func _ = 0

main :: IO ()
main = do
        print (ex21 (\x -> x) 12)
        print (ex21 test_func 1)
        print (ex21 test_func 2)
        print (ex21 test_func 3)
        print (ex21 test_func 4)
