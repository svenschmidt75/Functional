{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module EitherTSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck ( prop
                             , modifyMaxSuccess)
import Test.QuickCheck.Arbitrary ( Arbitrary
                                 , arbitrary)
import Test.QuickCheck.Gen (frequency)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic ( monadicIO
                               , run)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes ( functor
                               , applicative
                               , monad)

import Lib
    ( MyEitherT (..)
    )

instance (Arbitrary e, Arbitrary a, Monad m) => Arbitrary (MyEitherT e m a) where
--  arbitrary :: Gen a
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        frequency [
                    (1, return $ MyEitherT $ return (Right a))
                  , (1, return $ MyEitherT $ return (Left e))
                  ]

instance (Eq e, Eq a, Eq (Maybe (Either e a))) => EqProp (MyEitherT e Maybe a) where
    (=-=) = eq

spec :: Spec
spec = do
    describe "Functor laws" $
        modifyMaxSuccess (const 1) $
            prop "Checkers - Functor" functorCheckers
    describe "Functor laws" $
        modifyMaxSuccess (const 1) $
            prop "Checkers - Applicative" applicativeCheckers
    describe "Functor laws" $
        modifyMaxSuccess (const 1) $
            prop "Checkers - Monad" monadCheckers

functorCheckers :: Property
functorCheckers = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: MyEitherT String Maybe (Int, Int, Int)
    run $ quickBatch $ functor trigger

applicativeCheckers :: Property
applicativeCheckers = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: MyEitherT String Maybe (Int, Int, Int)
    run $ quickBatch $ applicative trigger

monadCheckers :: Property
monadCheckers = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: MyEitherT String Maybe (Int, Int, Int)
    run $ quickBatch $ monad trigger
