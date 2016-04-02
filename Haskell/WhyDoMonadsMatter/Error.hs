module Failure where

divBy x y = div y x

main = do
        print ((divBy 2 . divBy 5) 2310)
        print ((divBy 2 . divBy 7) 2310)
        print ((divBy 5 . divBy 11) 2310)
