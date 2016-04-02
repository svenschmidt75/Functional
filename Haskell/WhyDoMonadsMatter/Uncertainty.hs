module Uncertainty where

data Card = Ace
          | Number Integer
          | Picture

-- Ace can be either 1 or 11 later...
valueCard :: Card -> Integer
valueCard Ace        = 1
valueCard (Number x) = x
valueCard Picture    = 10

valueHand :: [Card] -> Integer
valueHand [] = 0
valueHand (c:cs) = (valueCard c) + valueHand cs


main = do
    print (valueHand [])
    print (valueHand [Ace, Number 2, Number 3])
    print (valueHand [Ace, Ace, Number 2])
    print (valueHand [Ace, Ace, Ace])
    print (valueHand [Picture, Picture])
    print (valueHand [Ace, Picture])
    print (valueHand [Ace, Picture, Picture])
    print (valueHand [Ace, Picture, Picture, Picture])
