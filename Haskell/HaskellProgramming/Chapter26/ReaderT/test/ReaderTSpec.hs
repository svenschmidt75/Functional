{-# LANGUAGE InstanceSigs #-}
module ReaderTSpec (spec) where

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
    ( MyReaderT (..)
    )

instance (Arbitrary a, Arbitrary b, Monad m) => Arbitrary (MyReaderT a m b) where
--  arbitrary :: Gen a
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [
                    (1, return $ MyEitherT $ return (Right b))
                  , (1, return $ MyEitherT $ return (Left a))
                  ]

instance (Eq a, Eq b, Eq (Maybe (Either a b))) => EqProp (MyEitherT a Maybe b) where
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
