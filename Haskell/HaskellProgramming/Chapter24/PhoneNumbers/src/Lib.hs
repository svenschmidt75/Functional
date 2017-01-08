module Lib
    ( PhoneNumber (..)
    ) where

import Data.Maybe
import Control.Applicative
import Text.Trifecta
import Text.Parser.LookAhead


type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = undefined
