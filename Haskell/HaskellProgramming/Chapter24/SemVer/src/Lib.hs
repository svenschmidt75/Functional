module Lib where
    -- ( SemVer (..)
    -- , NumberOrString (..)
    -- , parseSemVer
    -- ) where

import Data.Maybe
import Control.Applicative
import Text.Trifecta


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


numberParser :: Parser Integer
numberParser = read <$> some digit

stringParser :: Parser String
stringParser = some letter

numberOrStringParser :: Parser NumberOrString
numberOrStringParser =  NOSI <$> numberParser
                    <|> NOSS <$> stringParser

parsePreRelease :: Parser Release
parsePreRelease = do
    _ <- char '-'
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
    return $ SemVer major minor patch prerelease []
