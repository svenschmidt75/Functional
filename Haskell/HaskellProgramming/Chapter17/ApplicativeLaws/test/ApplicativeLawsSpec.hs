module ApplicativeLawsSpec (spec) where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Lib
    ( Bull (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


applicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
applicativeIdentity v = (pure id <*> v) == v

type IntToInt = Fun Int Int
type IntFC = Bull -> IntToInt -> IntToInt -> Bool

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools)
                          , (1, return Twoo) ]

spec :: Spec
spec = do
    describe "applicative laws" $ do
        prop "identity" $ do
            \x -> applicativeIdentity (x :: Bull)
{-
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: Identity Int)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "conditions" $ do
        it "condition 1" $ do
            let f = Combine $ \n -> Sum (n + 1)
            let result = unCombine (mappend f mempty) $ 1
            result `shouldBe` Sum 2
        prop "verify associativety" $ do
            semigroupAssoc :: BoolConjAssoc
}

prop5 :: Property
prop5 = monadicIO $ do
    let e = let ioi = readIO "1" :: IO Integer
                changed = (fmap read $ fmap ("123" ++) (fmap show ioi)) :: IO Integer
            in fmap (*3) changed
    result <- run $ e
    assert $ result == 3693
-}
