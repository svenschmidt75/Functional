module GetDogRMReaderSpec (spec) where

import Test.Hspec

import Lib


newtype HumanName = HumanName String
    deriving (Eq, Show)

newtype DogName = DogName String
    deriving (Eq, Show)

newtype Address = Address String
    deriving (Eq, Show)

data Person = Person { humanName :: HumanName
                     , dogName :: DogName
                     , address :: Address
                     }
    deriving (Eq, Show)

data Dog = Dog { dogsName :: DogName
               , dogsAddress :: Address
               }
    deriving (Eq, Show)

getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy



dogName' :: Reader Person DogName
dogName' = Reader $ dogName

address' :: Reader Person Address
address' = Reader $ address

getDogRM' :: Reader Person Dog
getDogRM' = do
    name <- dogName'
    addy <- address'
    return $ Dog name addy

spec :: Spec
spec = do
    describe "(->) r Functor" $ do
        it "simple" $ do
            let r = (Reader $ \a -> a + 1) :: Reader Int Int
            runReader ((+1) <$> r) 1 `shouldBe` 3
    describe "(->) r Monad" $ do
        it "Person Example" $ do
            let p = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")
            getDogRM p `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
        it "Person Example 2" $ do
            let p = Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")
            (runReader getDogRM' $ p) `shouldBe` Dog (DogName "Barkley") (Address "Sesame Street")
        it "Simple Example" $ do
            let p = (Reader $ \r -> r + 1) :: Reader Int Int
            let tmp = p >>= \a -> Reader $ \r -> a + r + 7
            runReader tmp 1 `shouldBe` 10
