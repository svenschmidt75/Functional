module Lib
    ( Two (..)
    ) where

import Data.Semigroup
import Data.Monoid hiding ((<>))


data Two a b = Two a b
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
{- We know nothing about types a and b, so we also cannot know
 - what the identity element for Two a b should be. Because of
 - this, we simply require a and b to be a monoid, because then
 - both a and b have an identity element. We then *assume* that
 - Two (identity :: a) (identity :: b) is the identity element
 - we require.
 - This works, because
 - Two (a <> identity) (b <> identity) = Two a b!
-}
    mempty  = Two mempty mempty
    mappend = (<>)
