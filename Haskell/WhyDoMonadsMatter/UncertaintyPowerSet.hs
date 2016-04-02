module Uncertainty where

data Card = Ace
          | Number Integer
          | Picture

valueCard :: Card -> P Integer
valueCard Ace        = [1, 11]
valueCard (Number x) = [x]
valueCard Picture    = [10]

valueHand :: [Card] -> P Integer
valueHand [] = [0]
valueHand (c:cs) = (valueCard c) `addP` valueHand cs


main = do
    print (valueHand [])
    print (valueHand [Ace, Number 2, Number 3])
    print (valueHand [Ace, Ace, Number 2])
    print (valueHand [Ace, Ace, Ace])
    print (valueHand [Picture, Picture])
    print (valueHand [Ace, Picture])
    print (valueHand [Ace, Picture, Picture])
    print (valueHand [Ace, Picture, Picture, Picture])

type P a = [a]

addP :: P Integer -> P Integer -> P Integer
addP xs ys = [ x + y | x <- xs, y <- ys ]
