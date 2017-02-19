module ShowIPv6
    ( showQuad
    ) where

import Data.Word
import Data.List
import Data.Bits
import qualified Text.Printf as TP


-- Group elements by consecutive index in list of tuples.
--  input: [(1,'0'),(2,'0'),(3,'0'),(4,'0'),(6,'0')]
-- output: [[(1,'0'),(2,'0'),(3,'0'),(4,'0')],[(6,'0')]]
groupByConsecutive :: [(Int, a)] -> [[(Int, a)]]
groupByConsecutive = groupByConsecutive' []
    where
        groupByConsecutive' :: [(Int, b)] -> [(Int, b)] -> [[(Int, b)]]
        groupByConsecutive' accum (x:y:ys) = if fst x + 1 == fst y then
                                                 groupByConsecutive' (accum ++ [x]) (y:ys)
                                             else
                                                (accum ++ [x]) : groupByConsecutive' [] (y:ys)
        groupByConsecutive' accum [x]      = [accum ++ [x]]
        groupByConsecutive' _     []       = [[]]

-- Find indices of longest consecutive list of 0 in a list of numbers.
-- Must be at least two 0 next to one another.
--  input: [65152,0,0,0,514,46079,65054,33577]
-- output: [1,2,3]
getLongestZeroRange :: [Word16] -> [Int]
getLongestZeroRange input =
            let p1 = zip [0..] input
                p2 = filter (\x -> snd x == 0) p1
                p3 = groupByConsecutive p2
                p4 = maximumBy (\a b -> compare (length a) (length b)) p3
                p5 = if length p4 < 2 then [] else p4
            in map fst p5

--  input: Word64 = [63..48 47..32 31..16 15..0]
-- output: [63..48, 47..32, 31..16, 15..0]
decompose64 :: Word64 -> [Word16]
decompose64 w64 = let w16_1 = fromInteger (toInteger w64 .&. 0xFFFF)
                      w16_2 = fromInteger (toInteger w64 `shiftR` 16) .&. 0xFFFF
                      w16_3 = fromInteger (toInteger w64 `shiftR` 32) .&. 0xFFFF
                      w16_4 = fromInteger (toInteger w64 `shiftR` 48) .&. 0xFFFF
                  in [w16_4, w16_3, w16_2, w16_1]

decompose128 :: Word64 -> Word64 -> [Word16]
decompose128 hw lw = concat [decompose64 hw, decompose64 lw]

removeLeadingZeros :: [Word16] -> [Word16]
removeLeadingZeros = dropWhile (== 0)

intersperse128 :: [Word16] -> [Int] -> [String]
intersperse128 ws [] = map f [0..length ws - 1]
    where
        f idx
          | idx == 0         = bitGroup
          | otherwise        = ":" ++ bitGroup
          where
              bitGroup = TP.printf "%x" (ws !! idx)
intersperse128 ws idxs = map f [0..length ws - 1]
    where
        f idx
          | idx == last idxs && idx == length ws - 1 = ":"
          | idx >= head idxs && idx < last idxs      = ""
          | idx == last idxs                         = ":"
          | idx == length ws - 1                     = bitGroup
          | otherwise                                = bitGroup ++ ":"
          where
              bitGroup = TP.printf "%x" (ws !! idx)

showQuad :: Word64 -> Word64 -> String
showQuad hw lw = let r1 = removeLeadingZeros $ decompose128 hw lw
                     r2 = intersperse128 r1 (getLongestZeroRange r1)
                 in concat r2
