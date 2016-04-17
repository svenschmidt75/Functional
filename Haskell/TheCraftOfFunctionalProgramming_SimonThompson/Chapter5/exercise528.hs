module Exercise528 where


type Person = String
type Book   = String

-- ("Alice", "Asterix") -> 'Alice' has borrowed book 'Asterix'
type Database = [(Person, Book)]

books :: Database -> Person -> [Book]
books db person = [b | (p, b) <- db, p == person]

borrowers :: Database -> Book -> [Person]
borrowers db book = [p | (p, b) <- db, b == book]

borrowed :: Database -> Book -> Bool
borrowed db book = case borrowers db book of
                    []    -> False
                    (_:_) -> True

numBorrowed :: Database -> Person -> Int
numBorrowed db person = length $ books db person

makeLoan :: Database -> Person -> Book -> Database
makeLoan db person book = (person, book) : db

removeFromList :: Eq a => a -> [a] -> [a]
removeFromList x xs = case xs of
    [] -> []
    (y:ys) -> if x == y then removeFromList x ys else y : removeFromList x ys

returnLoan :: Database -> Person -> Book -> Database
returnLoan db person book = case elem person (borrowers db book) of
                                True -> removeFromList (person, book) db
                                False -> db





main :: IO ()
main = do
    let tmp1 = books [("Alice", "Book1"), ("Alice", "Book2")] "Alice"
    print tmp1
    let tmp2 = borrowers [("Alice", "Book1"), ("Alice", "Book2")] "Book2"
    print tmp2
    let tmp3 = borrowed [("Alice", "Book1"), ("Alice", "Book2")] "Book4"
    print tmp3
    let tmp4 = numBorrowed [("Alice", "Book1"), ("Alice", "Book2")] "Alice"
    print tmp4
    let tmp5 = borrowers (makeLoan [("Alice", "Book1"), ("Alice", "Book2")] "Alice" "Book3") "Book4"
    print tmp5
    let tmp6 = makeLoan [("Alice", "Book1"), ("Alice", "Book2")] "Alice" "Book3"
    print tmp6
    let tmp7 = returnLoan tmp6 "Alice" "Book3"
    print tmp7
