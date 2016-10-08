module Lib
    ( Two (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


data Two a b = Two a b
    deriving Show

instance Semigroup (Two a b) where
    (<>) = undefined

instance Monoid (Two a b) where
    mempty  = undefined
    mappend = (<>)
