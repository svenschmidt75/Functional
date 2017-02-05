module Lib
    -- ( IPAddress6 (..)
    -- ) where
        where

import Data.Word
import Control.Applicative
import qualified Text.Trifecta as TF


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


type Sta = ([Word16], [Word16])


-- pre :: TF.Parser (Maybe Word16)
-- pre =
--     chunk <- TF.optional parseChunk
--     (w:) <$> pre



bla :: TF.Parser [Word16]
bla = do
    chunk <- parseChunk
    (TF.eof >> return [chunk]) <|> (TF.char ':' >> TF.notFollowedBy (TF.char ':') >> (chunk:) <$> bla)

 --  <|> (TF.eof >> return [chunk])

--    (TF.eof >> return [chunk]) <|> (TF.string "::" >> return [chunk]) <|> (TF.char ':' >> TF.notFollowedBy (TF.char ':') >> (chunk:) <$> bla)

post :: TF.Parser [Word16]
post = undefined

parseIPvAddress :: TF.Parser IPAddress6
parseIPvAddress = undefined

