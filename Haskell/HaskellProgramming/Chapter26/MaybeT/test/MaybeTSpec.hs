module MaybeTSpec (spec) where

import Data.Maybe (isNothing)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic ( monadicIO
                               , assert
                               , run)

import Lib
    ( MyMaybeT (..)
    )


spec :: Spec
spec = do
    describe "Functor laws" $ do
            prop "Just 1" $
                functorJustProp 1
            prop "Just 39" $
                functorJustProp 39
            prop "Nothing"
                functorNothingProp
    describe "Applicative laws" $ do
            prop "Just-Just"
                applicativeJustJustProp
            prop "Just-Nothing"
                applicativeJustNothingProp
            prop "Nothing-Just"
                applicativeNothingJustProp
            prop "Nothing-Nothing"
                applicativeNothingNothingProp

-- monadicIO :: PropertyM IO a -> Property
-- run :: Monad m => m a -> PropertyM m a
functorJustProp :: Int -> Property
functorJustProp a = monadicIO $ do
    let value = MyMaybeT $ (return . Just) 1 :: MyMaybeT IO Int
    let result = (+a) <$> value
    r <- run (runMyMaybeT result)
    assert $ r == Just (1 + a)

functorNothingProp :: Property
functorNothingProp = monadicIO $ do
    let value = MyMaybeT $ return Nothing :: MyMaybeT IO Int
    let result = (+1) <$> value
    r <- run $ runMyMaybeT result
    assert $ isNothing r

applicativeJustJustProp :: Property
applicativeJustJustProp = monadicIO $ do
    let mf = MyMaybeT $ (return . Just) (+1) :: MyMaybeT IO (Int -> Int)
    let ma = MyMaybeT $ (return . Just) 1 :: MyMaybeT IO Int
    let result = mf <*> ma
    r <- run $ runMyMaybeT result
    assert $ r == Just 2

applicativeJustNothingProp :: Property
applicativeJustNothingProp = monadicIO $ do
    let mf = MyMaybeT $ (return . Just) (+1) :: MyMaybeT IO (Int -> Int)
    let ma = MyMaybeT $ return Nothing :: MyMaybeT IO Int
    let result = mf <*> ma
    r <- run $ runMyMaybeT result
    assert $ isNothing r

applicativeNothingJustProp :: Property
applicativeNothingJustProp = monadicIO $ do
    let mf = MyMaybeT $ return Nothing :: MyMaybeT IO (Int -> Int)
    let ma = MyMaybeT $ (return . Just) 1 :: MyMaybeT IO Int
    let result = mf <*> ma
    r <- run $ runMyMaybeT result
    assert $ isNothing r

applicativeNothingNothingProp :: Property
applicativeNothingNothingProp = monadicIO $ do
    let mf = MyMaybeT $ return Nothing :: MyMaybeT IO (Int -> Int)
    let ma = MyMaybeT $ return Nothing :: MyMaybeT IO Int
    let result = mf <*> ma
    r <- run $ runMyMaybeT result
    assert $ isNothing r
