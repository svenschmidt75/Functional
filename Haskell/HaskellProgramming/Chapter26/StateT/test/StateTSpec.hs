{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module StateTSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck ( prop
                             , modifyMaxSuccess)
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic ( monadicIO
                               , run)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes ( functor
                               , applicative
                               , monad)

import Lib
    ( MyStateT (..)
    )

instance (Arbitrary s, Arbitrary a, Monad m) => Arbitrary (MyStateT s m a) where
--  arbitrary :: Gen a
    arbitrary = do
        a <- arbitrary
        return $ MyStateT (\_ -> return a)

instance (Monad m, Eq (MyStateT s m a)) => EqProp (MyStateT s m a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Functor laws" $
        modifyMaxSuccess (const 1) $
            prop "Checkers - Functor" functorCheckers
    -- describe "Applicative laws" $
    --     modifyMaxSuccess (const 1) $
    --         prop "Checkers - Applicative" applicativeCheckers
    -- describe "Monad laws" $
    --     modifyMaxSuccess (const 1) $
    --         prop "Checkers - Monad" monadCheckers

functorCheckers :: Property
functorCheckers = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: MyStateT Int Maybe (String, String, String)
    run $ quickBatch $ functor trigger

-- applicativeCheckers :: Property
-- applicativeCheckers = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: MyReaderT Int Maybe (Int, Int, Int)
--     run $ quickBatch $ applicative trigger

-- monadCheckers :: Property
-- monadCheckers = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: MyReaderT Int Maybe (Int, Int, Int)
--     run $ quickBatch $ monad trigger
