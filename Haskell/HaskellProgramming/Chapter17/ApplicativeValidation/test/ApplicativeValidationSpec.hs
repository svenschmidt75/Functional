module ApplicativeValidationSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Success, Failure)
import Test.QuickCheck.Function
import Test.QuickCheck (Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.QuickCheck.Checkers

import Lib
    ( Validation (..)
    )


functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)


applicativeIdentity :: (Applicative f, Eq (f a)) => f a -> Bool
applicativeIdentity v = (pure id <*> v) == v

{- What does "pure (.) <*> u <*> v <*> w == u <*> v <*> w" mean?
 - Let's look at the types...
 - (.) :: (b -> c) -> (a -> b) -> a -> c
 - f ~ Either e, so
 - pure :: a -> f        a
 - pure :: a -> Either e a
 - so
 - pure (.) lifts function (.) into the Either e context,
 - pure (.) :: Either e ((b -> c) -> (a -> b) -> a -> c) where a :: (b -> c) -> (a -> b) -> a -> c
 - Also,
 - <*> :: f        (x -> y) -> f        a -> f        b
 - <*> :: Either e (x -> y) -> Either e x -> Either e y
 - pure (.) <*> u = (<*>) (pure (.)) u :: Either e ?
 - The question is, what is x and y in 'Either e (x -> y)' the definition of <*>?
 - To see that, we use the fact that -> is right associative, so
 - (.) :: (b -> c) -> (a -> b) -> a -> c = (a -> b) -> ((a -> b) -> a -> c)
 - or (.) :: x -> y, where x = (b -> c) and y = ((a -> b) -> a -> c)
 - so
 - pure (.) <*> u :: Either e y = Either e ((a -> b) -> a -> c)
 - and
 - pure (.) <*> u <*> v :: Either e (a -> c)
 - and
 - pure (.) <*> u <*> v <*> w :: Either e c
 - Now "u <*> v <*> w":
 - u :: Either e (b -> c)
 - v :: Either e (a -> b)
 - w :: Either e a
 - so
 - v <*> w :: Either e b
 - and
 - u <*> (v <*> w) :: Either e c
 - where the function g :: b -> c in context u :: Either e g acts on
 - the value b in context v <*> w :: Either e c
 -}
-- We would have to be able to compare functions here?
--applicativeCompose' :: (Eq (f c), Applicative f) => f a -> Fun b c -> Fun a b -> Fun a c -> Bool
--applicativeCompose' x (Fun _ u) (Fun _ v) (Fun _ w) = (pure (.) <*> u <*> v <*> w) x == (u <*> (v <*> w)) x


data Errors = DividedByZero
            | StackOverflow
            | MooglesChewedWires
    deriving (Eq, Show)

type ValidationType = Validation [Errors] Int
type IntToInt = Fun Int Int
type IntFC = ValidationType -> IntToInt -> IntToInt -> Bool

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
    arbitrary = do
        a <- arbitrary
        e <- arbitrary
        frequency [
                    (1, return $ Success a)
                  , (1, return $ Failure e)
                  ]

instance Arbitrary Errors where
    arbitrary = frequency [
                            (1, return $ DividedByZero)
                          , (1, return $ StackOverflow)
                          , (1, return $ MooglesChewedWires)
                          ]

spec :: Spec
spec = do
    describe "functor laws" $ do
        prop "identity" $ do
            \x -> functorIdentity (x :: ValidationType)
        prop "composition 1" $ do
            let li = functorCompose (+1) (*2)
            \x -> li (x :: ValidationType)
        prop "composition 2" $ do
            functorCompose' :: IntFC
    describe "some general tests" $ do
        it "Failure - Failure" $ do
            Failure [MooglesChewedWires] <*> Failure [StackOverflow] `shouldBe` (Failure [MooglesChewedWires, StackOverflow] :: ValidationType)
        it "Failure - Success" $ do
            Failure [StackOverflow] <*> Success (+1) `shouldBe` (Failure [StackOverflow] :: ValidationType)
        it "Success - Failure" $ do
            Success (+1) <*> Failure [StackOverflow] `shouldBe` (Failure [StackOverflow] :: ValidationType)
        it "Success - Success" $ do
            Success (+1) <*> Success 1 `shouldBe` (Success 2 :: ValidationType)
    describe "applicative laws" $ do
        prop "identity" $ do
            \x -> applicativeIdentity (x :: ValidationType)
