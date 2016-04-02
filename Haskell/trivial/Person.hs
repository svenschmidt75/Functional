data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing
  deriving Show

-- Person is a type constructor taking 3 arguments
  
brent :: Person
brent = Person "Brent" 30 SealingWax

-- Person is also a data constructor!

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

--main = print (getAge brent)






baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

--main = putStrLn (baz brent)
--main = print (baz brent)






checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _)          = n ++ ", your favorite thing is lame."

--main = putStrLn (checkFav (Person "Brent" 30 SealingWax))
main = putStrLn (checkFav brent)
