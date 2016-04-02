{-
  My 1st definition of map looked like this:

  map' :: (a -> b) -> [a] -> [b]
  map' f xs = [u | x <- xs, u <- f x]

  This didn't work, because 'u <- f x' does not have type Bool.
  This form can be used to only select elements in the list
  comprehension that satify that condition.
-}
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

square :: (Num a) => a -> a
square x = x * x

list1 = [1, 2, 3, 4, 5]
list2 = ['a', 'b', 'c', 'd']

main = print (map' square list1)
