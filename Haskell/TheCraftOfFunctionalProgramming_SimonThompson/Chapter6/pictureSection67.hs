module SupermarketBill where

import Test.QuickCheck hiding (Result, scale)
import Prelude hiding (lookup)


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

formatPence :: Price -> String
formatPence price = let integer_part = price `div` 100 in
                    let fractional_part = price `mod` 100 in
                    let formatted_integer_part = show integer_part in
                    let formatted_fractional_part = show fractional_part in
                    formatted_integer_part ++ "." ++ (padWithZero formatted_fractional_part)
                    where
                     padWithZero formatted_fractional_part
                        | length formatted_fractional_part == 2 = formatted_fractional_part
                        | otherwise                             = (replicate (2 - length formatted_fractional_part) '0') ++ formatted_fractional_part

formatLine :: (Name, Price) -> String
formatLine (name, price) = let ln = length name in
                           let lp = length (formatPence price) in
                           let diff = lineLength - (ln + lp) in
                           name ++ replicate diff '.' ++ (formatPence price) ++ "\n"

formatLines :: [(Name, Price)] -> String
--formatLines (x:xs) = formatLine x ++ formatLines xs
--formatLines []     = ""
formatLines items = foldr (\item accum -> accum ++ formatLine item) "" items


makeDiscount :: BillType -> Price
makeDiscount bill = let nSherries = length (filter (\(name, _) -> name == "Dry Sherry, 1lt") bill) in
                    100 * (nSherries - 1)

formatDiscount :: Price -> String
formatDiscount price = "\n" ++ formatLine ("Discount", price)


makeTotal :: BillType -> Price
makeTotal ((_, price):xs) = price + makeTotal xs
makeTotal []              = 0

formatTotal :: Price -> String
formatTotal price = "\n" ++ formatLine ("Total", price)

formatBill :: BillType -> String
formatBill bill = let mainBill = formatLines bill in
                  let discount = makeDiscount bill in
                  let total    = formatTotal (makeTotal bill - discount) in
                  if discount == 0 then
                    mainBill ++ total
                  else
                    mainBill ++ formatDiscount discount ++ total

look :: DataBase -> BarCode -> (Name, Price)
look ((bc, name, price):xs) barCode = if barCode == bc then (name, price) else look xs barCode
look                     []       _ = ("Unknown Item", 0)

lookup :: BarCode -> (Name, Price)
lookup barCode = look codeIndex barCode

makeBill :: TillType -> BillType
makeBill till = map lookup till

produceBill :: TillType -> String
produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30

removeBarCode :: DataBase -> BarCode -> DataBase
removeBarCode ((barcode, name, price):dbs) barCode
    | barcode == barCode = removeBarCode dbs barCode
    | otherwise          = (barcode, name, price) : removeBarCode dbs barCode
removeBarCode                           []       _ = []

addBarCode :: DataBase -> (BarCode, Name, Price) -> DataBase
addBarCode db item = item : db

updateDataBase :: DataBase -> (BarCode, Name, Price) -> DataBase
updateDataBase db (barCode, name, price) = let updatedDataBase = removeBarCode db barCode in
                                           addBarCode updatedDataBase (barCode, name, price)

main :: IO ()
main = do
    putStrLn $ produceBill [1234, 4719, 3814, 1112, 9, 1234]
