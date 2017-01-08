module Lib where
    ( SemVer (..)
    , NumberOrString (..)
    , parseSemVer
    ) where

import Data.Maybe
import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead


-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString = NOSS String
                    | NOSI Integer
    deriving (Show, Eq)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
    deriving (Show, Eq)

instance Ord SemVer where
    (<=) (SemVer major1 minor1 patch1 release1 _) (SemVer major2 minor2 patch2 release2 _) =
        -- need to deal with release, but that is somewhat complicated...
        major1 <= major2 && minor1 <= minor2 && patch1 <= patch2

parseDelimiter :: Parser ()
parseDelimiter = do
    eof <|> (lookAhead (char '.') >> return ())

numberParser :: Parser Integer
numberParser = read <$> do
    d <- some digit
    parseDelimiter
    return d

stringParser :: Parser String
stringParser = some alphaNum

numberOrStringParser :: Parser NumberOrString
numberOrStringParser =  (try $ NOSI <$> numberParser)
                    <|> NOSS <$> stringParser

parsePreRelease :: Parser Release
parsePreRelease = do
    _ <- char '-'
    sepBy numberOrStringParser (char '.')

parseMetadata :: Parser Metadata
parseMetadata = do
    _ <- char '+'
    sepBy numberOrStringParser (char '.')

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- integer
    _ <- char '.'
    minor <- integer
    _ <- char '.'
    patch <- integer
    maybePrerelease <- optional parsePreRelease
    let prerelease = fromMaybe [] maybePrerelease
    maybeMetadata <- optional parseMetadata
    let metadata = fromMaybe [] maybeMetadata
    return $ SemVer major minor patch prerelease metadata
