n = case "Hello" of
  []      -> 3
  ('H':s) -> length s
  _       -> 7

--main = print n








data FailableDouble = Failure 
                    | OK Double
  deriving Show

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK d    -> d
  
main = print (failureToZero' Failure, failureToZero' (OK 3.4))
