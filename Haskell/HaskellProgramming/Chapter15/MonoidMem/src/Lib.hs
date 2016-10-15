module Lib
    ( Mem (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


-- SS on 10/15/2016: This looks like the State monad...
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup a => Semigroup (Mem s a) where
    f <> g = Mem $ \s -> let (a', s')  = runMem f s
                             (a'', s'') = runMem g s'
                           in (a' <> a'', s'')

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
    mempty  = Mem $ \s -> (mempty, s)
    mappend = (<>)
