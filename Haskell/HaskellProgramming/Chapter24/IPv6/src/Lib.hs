module Lib
    -- ( IPAddress6 (..)
    -- ) where
        where

import Data.Word
import Data.Bits
import Control.Applicative
import qualified Text.Trifecta as TF
import qualified Text.Parser.LookAhead as PL


data IPAddress6 = IPAddress6 Word64 Word64
    deriving (Eq, Ord, Show)

parseBitGroup :: TF.Parser Word16
parseBitGroup = do
    digits <- TF.some TF.hexDigit
    let value = read ("0x" ++ digits) :: Integer
    let maxValue = toInteger (maxBound :: Word16)
    if value > maxValue then
        TF.unexpected "a chunk must not have more than 4 hex digits"
    else
        return $ fromInteger value

parseBitGroups :: TF.Parser [Word16]
parseBitGroups = do
    bitGroup <- parseBitGroup
    -- We need to account for 3 possibilities:
    -- 1. :: in which case we are done
    -- 2. : in which case we expect the next bitGroup
    -- 3. eof, end of IPv6 address
    (PL.lookAhead (TF.string "::") >> return []) <|> (TF.eof >> return [bitGroup]) <|> (TF.char ':' >> TF.notFollowedBy (TF.char ':') >> (bitGroup:) <$> parseBitGroups)

parseIPv6Address :: TF.Parser IPAddress6
parseIPv6Address = do
    upper4BitGroups <- parseBitGroups
    lower4BitGroups <- (TF.eof >> return []) <|> (TF.string "::" >> parseBitGroups)
    let expanded = expand upper4BitGroups lower4BitGroups
    let upperExpanded = combine (take 4 expanded)
    let lowerExpanded = combine (drop 4 expanded)
    return $ IPAddress6 upperExpanded lowerExpanded

expand :: [Word16] -> [Word16] -> [Word16]
expand p1 p2 =
    let zeros = [0 | _ <- [1..(8 - (length p1 + length p2))]]
    in p1 ++ zeros ++ p2

combine :: [Word16] -> Word64
combine xs =
    let bitGroup1 = fromIntegral $ head xs
        bitGroup2 = fromIntegral $ xs !! 1
        bitGroup3 = fromIntegral $ xs !! 2
        bitGroup4 = fromIntegral $ xs !! 3
        dec       = (bitGroup1 `shiftL` 48) .|. (bitGroup2 `shiftL` 32) .|. (bitGroup3 `shiftL` 16) .|. bitGroup4
    in dec
