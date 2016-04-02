-- Data types can be recursive, that is, defined in terms of themselves.

-- IntList is defined using itself, i.e. Cons Int IntList
-- ^^^^^^^                                        ^^^^^^^
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x xs) = x * intListProd xs

main = print (intListProd (Cons 3 (Cons 2 (Cons 4 Empty))))
