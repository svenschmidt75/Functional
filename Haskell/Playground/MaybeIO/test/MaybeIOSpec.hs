module MaybeIOSpec (spec) where

import Data.Maybe (isNothing)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic ( monadicIO
                               , assert
                               , run)

import Lib
    ( MaybeIO (..)
    )


-- instance Eq a => EqProp (MaybeIO a) where
--     (=-=) = eq

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

    -- describe "verify laws" $ do
    --     modifyMaxSuccess (const 1) $ do
    --         prop "MaybeIO Functor laws" $ do
    --             maybeIOFunctorLawsProp
            -- prop "Sum Applicative laws" $ do
            --     sumApplicativeLawsProp
            -- prop "Sum Monad laws" $ do
            --     sumMonadLawsProp
-- maybeIOFunctorLawsProp :: Property
-- maybeIOFunctorLawsProp = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: MaybeIO (Int, Int, Int)
--     run $ quickBatch $ functor trigger

-- sumApplicativeLawsProp :: Property
-- sumApplicativeLawsProp = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: Sum String (Int, Int, Int)
--     run $ quickBatch $ applicative trigger

-- sumMonadLawsProp :: Property
-- sumMonadLawsProp = monadicIO $ do
--     -- result type is Test.QuickCheck.Monadic.PropertyM IO ()
--     let trigger = undefined :: Sum String (Int, Int, Int)
--     run $ quickBatch $ monad trigger

-- monadicIO :: PropertyM IO a -> Property
-- run :: Monad m => m a -> PropertyM m a
functorJustProp :: Int -> Property
functorJustProp a = monadicIO $ do
    let value = MaybeIO $ (return . Just) 1 :: MaybeIO Int
    let result = (+a) <$> value
    r <- run (runMaybeIO result)
    assert $ r == Just (1 + a)

functorNothingProp :: Property
functorNothingProp = monadicIO $ do
    let value = MaybeIO $ return Nothing :: MaybeIO Int
    let result = (+1) <$> value
    r <- run $ runMaybeIO result
    assert $ isNothing r

applicativeJustJustProp :: Property
applicativeJustJustProp = monadicIO $ do
    let mf = MaybeIO $ (return . Just) (+1) :: MaybeIO (Int -> Int)
    let ma = MaybeIO $ (return . Just) 1 :: MaybeIO Int
    let result = mf <*> ma
    r <- run $ runMaybeIO result
    assert $ r == Just 2

applicativeJustNothingProp :: Property
applicativeJustNothingProp = monadicIO $ do
    let mf = MaybeIO $ (return . Just) (+1) :: MaybeIO (Int -> Int)
    let ma = MaybeIO $ return Nothing :: MaybeIO Int
    let result = mf <*> ma
    r <- run $ runMaybeIO result
    assert $ isNothing r

applicativeNothingJustProp :: Property
applicativeNothingJustProp = monadicIO $ do
    let mf = MaybeIO $ return Nothing :: MaybeIO (Int -> Int)
    let ma = MaybeIO $ (return . Just) 1 :: MaybeIO Int
    let result = mf <*> ma
    r <- run $ runMaybeIO result
    assert $ isNothing r

applicativeNothingNothingProp :: Property
applicativeNothingNothingProp = monadicIO $ do
    let mf = MaybeIO $ return Nothing :: MaybeIO (Int -> Int)
    let ma = MaybeIO $ return Nothing :: MaybeIO Int
    let result = mf <*> ma
    r <- run $ runMaybeIO result
    assert $ isNothing r
