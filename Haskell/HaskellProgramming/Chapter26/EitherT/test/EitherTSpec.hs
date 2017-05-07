{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module EitherTSpec (spec) where

import Data.Maybe (isNothing)
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
import Test.QuickCheck.Classes (functor)
import Data.Either (isLeft)
import Data.Maybe (fromJust)

import Lib
    ( MyEitherT (..)
    )

instance (Arbitrary a, Arbitrary b, Monad m) => Arbitrary (MyEitherT a m b) where
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
    describe "Functor laws" $ do
            prop "Just 1" $
                functorJustProp 1
            prop "Just 39" $
                functorJustProp 39
            prop "Nothing"
                functorNothingProp
            modifyMaxSuccess (const 1) $ do
                prop "Checkers - Functor"
                    sTraversableLawsProp
    -- describe "Applicative laws" $ do
    --         prop "Just-Just"
    --             applicativeJustJustProp
    --         prop "Just-Nothing"
    --             applicativeJustNothingProp
    --         prop "Nothing-Just"
    --             applicativeNothingJustProp
    --         prop "Nothing-Nothing"
    --             applicativeNothingNothingProp

-- monadicIO :: PropertyM IO a -> Property
-- run :: Monad m => m a -> PropertyM m a
functorJustProp :: Int -> Bool
functorJustProp a = do
    let value = MyEitherT $ (return . Right) 1 :: MyEitherT String Maybe Int
    let result = (+a) <$> value
    runMyEitherT result == Just (Right (1 + a))

sTraversableLawsProp :: Property
sTraversableLawsProp = monadicIO $ do
    -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
    let trigger = undefined :: MyEitherT String Maybe (Int, Int, Int)
    run $ quickBatch $ functor trigger

functorNothingProp :: Bool
functorNothingProp = do
    let value = MyEitherT $ (return $ Left "Error") :: MyEitherT String Maybe Int
    let result = (+1) <$> value
    isLeft (fromJust (runMyEitherT result))

-- applicativeJustJustProp :: Property
-- applicativeJustJustProp = monadicIO $ do
--     let mf = MyEitherT Int $ (return . Just) (+1) :: MyEitherT Int IO (Int -> Int)
--     let ma = MyEitherT Int $ (return . Just) 1 :: MyEitherT Int IO Int
--     let result = mf <*> ma
--     r <- run $ runMyEitherT result
--     assert $ r == Just 2

-- applicativeJustNothingProp :: Property
-- applicativeJustNothingProp = monadicIO $ do
--     let mf = MyEitherT Int $ (return . Just) (+1) :: MyEitherT Int IO (Int -> Int)
--     let ma = MyEitherT Int $ return Nothing :: MyEitherT Int IO Int
--     let result = mf <*> ma
--     r <- run $ runMyEitherT result
--     assert $ isNothing r

-- applicativeNothingJustProp :: Property
-- applicativeNothingJustProp = monadicIO $ do
--     let mf = MyEitherT Int $ return Nothing :: MyEitherT Int IO (Int -> Int)
--     let ma = MyEitherT Int $ (return . Just) 1 :: MyEitherT Int IO Int
--     let result = mf <*> ma
--     r <- run $ runMyEitherT result
--     assert $ isNothing r

-- applicativeNothingNothingProp :: Property
-- applicativeNothingNothingProp = monadicIO $ do
--     let mf = MyEitherT Int $ return Nothing :: MyEitherT Int IO (Int -> Int)
--     let ma = MyEitherT Int $ return Nothing :: MyEitherT Int IO Int
--     let result = mf <*> ma
--     r <- run $ runMyEitherT result
--     assert $ isNothing r
