module Lib
    ( Combine (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup (Mem s a) where
    (<>) = undefined

instance Monoid a => Monoid (Mem s a) where
    mempty  = undefined
    mappend = (<>)
