module Picture where

import Test.QuickCheck hiding (Result, scale)

type Name = String
type Price = Int
type BarCode = Int

type DataBase = [(BarCode, Name, Price)]

codeIndex :: DataBase
codeIndex = [(4719, "Fish Fingers",       121),
             (5643, "Nappies",            1010),
             (3814, "Orange Jelly",       56),
             (1111, "Hula Hoops",         21),
             (1112, "Hula Hoops (Giant)", 133),
             (1234, "Dry Sherry, 1lt",    540)]

type TillType = [BarCode]
type BillType = [(Name, Price)]

makeBill :: TillType -> BillType

formatBill :: BillType -> String

produceBill :: TillType -> String
produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30





main :: IO ()
main = do
    putStrLn "Test"
