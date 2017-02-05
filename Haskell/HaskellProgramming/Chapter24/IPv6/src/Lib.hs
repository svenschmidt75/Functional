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

parseChunk :: TF.Parser Word16
parseChunk = do
    digits <- TF.some TF.hexDigit
    let value = read ("0x" ++ digits) :: Integer
    let maxValue = toInteger (maxBound :: Word16)
    if value > maxValue then
        TF.unexpected "a chunk must not have more than 4 hex digits"
    else
        return $ fromInteger value

pre :: TF.Parser [Word16]
pre = do
    chunk <- parseChunk
    -- 3 possibilities:
    -- 1. :: in which case we are done
    -- 2. : in which case we expect the next chunk
    -- 3. eof, end of IPv6 address
    (PL.lookAhead (TF.string "::") >> return []) <|> (TF.eof >> return [chunk]) <|> (TF.char ':' >> TF.notFollowedBy (TF.char ':') >> (chunk:) <$> pre)

post :: TF.Parser [Word16]
post = pre

parseIPv6Address :: TF.Parser IPAddress6
parseIPv6Address = do
    pres <- pre
    posts <- (TF.eof >> return []) <|> (TF.string "::" >> post)
    let expanded = expand pres posts
    let pres2 = combine (take 4 expanded)
    let posts2 = combine (drop 4 expanded)
    return $ IPAddress6 pres2 posts2

expand :: [Word16] -> [Word16] -> [Word16]
expand pres posts =
    let p1 = pres
        p2 = posts
        ptmp = [0 | _ <- [1..(8 - (length p1 + length p2))]]
    in p1 ++ ptmp ++ p2

combine :: [Word16] -> Word64
combine xs =
    let chunk1 = fromIntegral $ head xs
        chunk2 = fromIntegral $ xs !! 1
        chunk3 = fromIntegral $ xs !! 2
        chunk4 = fromIntegral $ xs !! 3
        dec = (chunk1 `shiftL` 48) .|. (chunk2 `shiftL` 32) .|. (chunk3 `shiftL` 16) .|. chunk4
    in dec
