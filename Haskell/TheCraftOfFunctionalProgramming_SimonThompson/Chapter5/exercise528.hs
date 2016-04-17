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


{-
numBorrowed :: Database -> Person -> Int
makeLoad :: Database -> Person -> Book -> Database
returnLoad :: Database -> Person -> Book -> Database
-}





main :: IO ()
main = do
    let tmp1 = books [("Alice", "Book1"), ("Alice", "Book2")] "Alice"
    print tmp1
    let tmp2 = borrowers [("Alice", "Book1"), ("Alice", "Book2")] "Book2"
    print tmp2
    let tmp3 = borrowed [("Alice", "Book1"), ("Alice", "Book2")] "Book4"
    print tmp3
