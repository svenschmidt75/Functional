data FailableDouble = Failure 
                    | OK Double
  deriving Show

-- The type FailableDouble is defines an enum and has two data constructors:
-- the 1st does not take any argument
-- the 2nd constructs a value of type OK with the 1st argument passed, i.e.
-- OK 3.4 is a value of type FailableDouble. OK itself is not of type FailableDouble

-- :t OK
-- OK :: Double -> FailableDouble

{-
a = Failure
b = OK 3.4

main = print (a, b)
-}


{-
safeDiv :: Double -> Double -> FailableDouble
-- safeDiv :: (Double -> Double) -> FailableDouble
-- i.e. safeDiv takes two Doubles as argument and returns a FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

main = print (safeDiv 2 0, safeDiv 3 4)
-}


failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d) = d

main = print (failureToZero Failure, failureToZero (OK 3.4))