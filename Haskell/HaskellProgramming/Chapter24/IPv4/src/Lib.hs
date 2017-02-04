module Lib
    -- ( Identity (..)
    -- ) where
    where

import Data.Word
import Data.Bits
import Data.Char()
import qualified Text.Trifecta as TF


data IPAddress = IPAddress Word32
    deriving (Eq, Ord, Show)


parseOctet :: TF.Parser Word32
parseOctet = do
    octet <- TF.some TF.digit
    let value = read octet :: Integer
    let maxValue = toInteger (maxBound :: Word8)
    if (value > maxValue) then
        TF.unexpected "an octet must not have more than 3 digits"
    else
        return $ fromInteger value

parseIPvAddress :: TF.Parser IPAddress
parseIPvAddress = do
    octet1 <- parseOctet

    _ <- TF.char '.'
    octet2 <- parseOctet

    _ <- TF.char '.'
    octet3 <- parseOctet

    _ <- TF.char '.'
    octet4 <- parseOctet

    let dec = (octet1 `shiftL` 24) .|. (octet2 `shiftL` 16) .|. (octet3 `shiftL` 8) .|. octet4
    return $ IPAddress dec
