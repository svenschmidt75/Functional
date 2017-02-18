module ShowIPv6 where

import Data.Word
import Data.List
import Data.Bits
import qualified Numeric as N


-- Group by consecutive index in list of tuples.
--  input: [(1,'0'),(2,'0'),(3,'0'),(4,'0'),(6,'0')]
-- output: [[(1,'0'),(2,'0'),(3,'0'),(4,'0')],[(6,'0')]]
groupByConsecutive :: [(Integer, a)] -> [[(Integer, a)]]
groupByConsecutive = groupByConsecutive' []
    where
        groupByConsecutive' :: [(Integer, b)] -> [(Integer, b)] -> [[(Integer, b)]]
        groupByConsecutive' accum (x:y:ys) = if fst x + 1 == fst y then
                                                 groupByConsecutive' (accum ++ [x]) (y:ys)
                                             else
                                                (accum ++ [x]) : groupByConsecutive' [] (y:ys)
        groupByConsecutive' accum [x]      = [accum ++ [x]]
        groupByConsecutive' _     []       = error "should not happen"

-- input: "10002"
-- output: [(1,0), (2,0), (3,0), (4,0)]
getLongestZeroRange :: [Char] -> [(Integer, Char)]
getLongestZeroRange input =
            let p1 = zip [0..] input
                p2 = filter (\x -> snd x == '0') p1
                p3 = groupByConsecutive p2
                p4 = maximumBy (\a b -> compare (length a) (length b)) p3
                p5 = if length p4 < 2 then [] else p4
            in p5

-- input: "1000212"
-- output: 1:::212"
generateNormalized :: [Char] -> [(Integer, Char)] -> [Char]
generateNormalized input zeroRange =
                      let p1 = zip [0..] input
                          p4 = map f p1
                      in p4
                      where
                          p2 = fst $ head zeroRange
                          p3 = fst $ last zeroRange
                          f (idx, val) = if idx >= p2 && idx <= p3 then
                                             ':'
                                         else
                                             val

-- Compress :
-- input: 1000101
-- ouput: 1::1:0:1
{-
    let data2 = "10000010"
    let data4 = generateNormalized data2 (getLongestZeroRange data2)
    let data5 = formatIPv6 (intersperse ':' data4)
-}
formatIPv6 :: [Char] -> [Char]
formatIPv6 input = formatIPv6' [] input
    where
        formatIPv6' :: [Char] -> [Char] -> [Char]
        formatIPv6' accum (x:y:ys)
          | x == ':' && y == ':' = formatIPv6' accum (y:ys)
          | x /= ':' && y == ':' = formatIPv6' (accum ++ [x, y]) (y:ys)
          | x == ':' && y /= ':' = formatIPv6' (accum ++ [x, y]) ys
          | x /= ':' && y /= ':' = formatIPv6' (accum ++ [x, y]) (y:ys)
        formatIPv6' accum (x:xs) = formatIPv6' (accum ++ [x]) xs
        formatIPv6' accum [] = accum




decompose :: Word64 -> [Word16]
decompose w64 = let w16_1 = fromInteger (toInteger w64 .&. 0xFFFF)
                    w16_2 = fromInteger (toInteger w64 `shiftR` 16) .&. 0xFFFF
                    w16_3 = fromInteger (toInteger w64 `shiftR` 32) .&. 0xFFFF
                    w16_4 = fromInteger (toInteger w64 `shiftR` 48) .&. 0xFFFF
                in [w16_4, w16_3, w16_2, w16_1]

decomposeQuad :: Word64 -> Word64 -> [Word16]
decomposeQuad hw lw = concat [decompose hw, decompose lw]

ipV6ToString :: Word64 -> Word64 -> [String]
ipV6ToString hw lw = map (\x -> N.showHex x "") $ decomposeQuad hw lw
