module FunctorIdentityCoArbitrarySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Property as QCP

import Lib
    ( Identity (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = Identity Int -> IntToInt -> IntToInt -> Bool


instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: Identity Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
        prop "composition 2" $ do
            prop_commutativeAdd
        prop "composition 2" $ do
            prop_commutativeAdd2
        prop "composition 2" $ do
            prop_functorCompose

prop_commutativeAdd :: Gen QCP.Result
prop_commutativeAdd = do
  (x, y) <- arbitrary :: Gen (Int, Int)
  return $ if x + y == y + x
    then succeeded
    else failed { QCP.reason = "stupid non-commutative addition" }

prop_commutativeAdd2 :: Int -> Int -> Bool
prop_commutativeAdd2 x y = x + y == y + x

prop_functorCompose :: Gen QCP.Result
prop_functorCompose = do
    f <- arbitrary :: Gen (Int -> Int)
    g <- arbitrary :: Gen (Int -> Int)
    x <- arbitrary :: Gen (Identity Int)
    return $ if functorCompose f g x == True
    then succeeded
    else failed { QCP.reason = "stupid non-commutative addition" }

prop5 :: Property
prop5 = monadicIO $ do
    let e = let ioi = readIO "1" :: IO Integer
                changed = (fmap read $ fmap ("123" ++) (fmap show ioi)) :: IO Integer
            in fmap (*3) changed
    result <- run $ e
    assert $ result == 3693
