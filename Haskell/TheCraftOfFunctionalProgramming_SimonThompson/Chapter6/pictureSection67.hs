module SupermarketBill where

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

makeTotal :: BillType -> Price
makeTotal ((_, price):xs) = price + makeTotal xs
makeTotal []              = 0

formatTotal :: Price -> String
formatTotal price = "\n" ++ formatLine ("Total", price)

formatBill :: BillType -> String
formatBill bill = formatLines bill ++ formatTotal (makeTotal bill)



--makeBill :: TillType -> BillType


--produceBill :: TillType -> String
--produceBill = formatBill . makeBill

lineLength :: Int
lineLength = 30





main :: IO ()
main = do
    putStrLn "Test"
