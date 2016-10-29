module Lib
    ( Constant (..)
    ) where


newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
-- f ~ Constant e
-- Functor f =>
-- fmap :: (a -> b) -> Constant e a -> Constant e b
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
-- f ~ Constant e
-- Applicative f =>
-- pure :: a -> f          a
-- pure :: a -> Constant e a
    pure _ = Constant mempty

-- (<*>) :: f          (a -> b) -> f          a -> f          b
-- (<*>) :: Constant e (a -> b) -> Constant e a -> Constant e b
    (<*>) (Constant a) (Constant b) = Constant $ a `mappend` b
