module Lib
    ( Optional (..)
    ) where

data Optional a = Nada
                | Only a
        deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    -- identity
    mempty = Nada

    {- Because the Optional data type is parametrically polymorphic in a,
       we have to express its monoidal properties in terms of the monoidal
       properties of the datatype a itself. How else would it make sense
       to define the binary relation?
    -}
    (Only x) `mappend` (Only y) = Only (x `mappend` y)
    (Only x) `mappend` Nada     = Only x
    Nada     `mappend` (Only x) = Only x
    Nada     `mappend` Nada     = Nada
