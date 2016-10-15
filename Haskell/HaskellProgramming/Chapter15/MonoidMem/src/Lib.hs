module Lib
    ( Mem (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


-- SS on 10/15/2016: This looks like the State monad...
newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance Semigroup (Mem s a) where
    f1 <> f2 = Mem $ \s -> let (a1, s1) = runMem f1 s in
                               (a1, s1)


instance Monoid a => Monoid (Mem s a) where
    mempty  = undefined
    mappend = (<>)
