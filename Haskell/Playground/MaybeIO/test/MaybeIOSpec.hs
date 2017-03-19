module MaybeIOSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck ( modifyMaxSuccess
                             , prop)
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic ( monadicIO
                               , run)

import Lib
    ( MaybeIO (..)
    )


-- instance Eq a => EqProp (MaybeIO a) where
--     (=-=) = eq

spec :: Spec
spec = do
    describe "Functor laws" $ do
            prop "Nothing" $ do
                functorNothingProp

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
functorNothingProp :: Property
functorNothingProp = monadicIO $ do
    let value = MaybeIO $ (return . Just) 1
    run $ runMaybeIO value
